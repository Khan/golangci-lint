package goanalysis

import (
	"fmt"
	"go/token"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/packages"

	"github.com/golangci/golangci-lint/pkg/goanalysis/pkgerrors"
	"github.com/golangci/golangci-lint/pkg/lint/linter"
	"github.com/golangci/golangci-lint/pkg/logutils"
	"github.com/golangci/golangci-lint/pkg/result"
	"github.com/golangci/golangci-lint/pkg/timeutils"
)

type runAnalyzersConfig interface {
	getName() string
	getLinterNameForDiagnostic(*Diagnostic) string
	getAnalyzers() []*analysis.Analyzer
	useOriginalPackages() bool
	reportIssues(*linter.Context) []Issue
	getLoadMode() LoadMode
}

func runAnalyzers(cfg runAnalyzersConfig, lintCtx *linter.Context) ([]result.Issue, error) {
	log := lintCtx.Log.Child(logutils.DebugKeyGoAnalysis)
	sw := timeutils.NewStopwatch("analyzers", log)

	const stagesToPrint = 10
	defer sw.PrintTopStages(stagesToPrint)

	runner := newRunner(cfg.getName(), log, lintCtx.PkgCache, lintCtx.LoadGuard, cfg.getLoadMode(), sw)

	pkgs := lintCtx.Packages
	if cfg.useOriginalPackages() {
		pkgs = lintCtx.OriginalPackages
	}

	issues, pkgsFromCache := loadIssuesFromCache(pkgs, lintCtx, cfg.getAnalyzers())
	var pkgsToAnalyze []*packages.Package
	for _, pkg := range pkgs {
		if !pkgsFromCache[pkg] {
			pkgsToAnalyze = append(pkgsToAnalyze, pkg)
		}
	}

	diags, errs, passToPkg := runner.run(cfg.getAnalyzers(), pkgsToAnalyze)

	defer func() {
		if len(errs) == 0 {
			// If we try to save to cache even if we have compilation errors
			// we won't see them on repeated runs.
			saveIssuesToCache(pkgs, pkgsFromCache, issues, lintCtx, cfg.getAnalyzers())
		}
	}()

	buildAllIssues := func() []result.Issue {
		var retIssues []result.Issue
		reportedIssues := cfg.reportIssues(lintCtx)
		for i := range reportedIssues {
			issue := &reportedIssues[i].Issue
			if issue.Pkg == nil {
				issue.Pkg = passToPkg[reportedIssues[i].Pass]
			}
			retIssues = append(retIssues, *issue)
		}
		retIssues = append(retIssues, buildIssues(diags, cfg.getLinterNameForDiagnostic)...)
		return retIssues
	}

	errIssues, err := pkgerrors.BuildIssuesFromIllTypedError(errs, lintCtx)
	if err != nil {
		return nil, err
	}

	issues = append(issues, errIssues...)
	issues = append(issues, buildAllIssues()...)

	return issues, nil
}

func buildIssues(diags []Diagnostic, linterNameBuilder func(diag *Diagnostic) string) []result.Issue {
	var issues []result.Issue
	for i := range diags {
		diag := &diags[i]
		linterName := linterNameBuilder(diag)

		issues = append(issues, buildSingleIssue(diag, linterName))

		if len(diag.Related) > 0 {
			for _, info := range diag.Related {
				issues = append(issues, result.Issue{
					FromLinter: linterName,
					Text:       fmt.Sprintf("%s(related information): %s", diag.Analyzer.Name, info.Message),
					Pos:        diag.Pkg.Fset.Position(info.Pos),
					Pkg:        diag.Pkg,
				})
			}
		}
	}
	return issues
}

func buildSingleIssue(diag *Diagnostic, linterName string) result.Issue {
	text := generateIssueText(diag, linterName)
	issue := result.Issue{
		FromLinter: linterName,
		Text:       text,
		Pos:        diag.Position,
		Pkg:        diag.Pkg,
	}

	if len(diag.SuggestedFixes) > 0 {
		// Don't really have a better way of picking a best fix right now
		chosenFix := diag.SuggestedFixes[0]

		// It could be confusing to return more than one issue per single diagnostic,
		// but if we return a subset it might be a partial application of a fix. Don't
		// apply a fix unless there is only one for now
		if len(chosenFix.TextEdits) == 1 {
			edit := chosenFix.TextEdits[0]

			pos := diag.Pkg.Fset.Position(edit.Pos)
			end := diag.Pkg.Fset.Position(edit.End)

			newLines := strings.Split(string(edit.NewText), "\n")

			// This only works if we're only replacing whole lines with brand new lines
			if onlyReplacesWholeLines(pos, end, newLines) {
				// both original and new content ends with newline, omit to avoid partial line replacement
				newLines = newLines[:len(newLines)-1]

				issue.Replacement = &result.Replacement{NewLines: newLines}
				issue.LineRange = &result.Range{From: pos.Line, To: end.Line - 1}

				return issue
			}
		}
	}

	return issue
}

func onlyReplacesWholeLines(oPos token.Position, oEnd token.Position, newLines []string) bool {
	return oPos.Column == 1 && oEnd.Column == 1 &&
		oPos.Line < oEnd.Line && // must be replacing at least one line
		newLines[len(newLines)-1] == "" // edit.NewText ended with '\n'
}

func generateIssueText(diag *Diagnostic, linterName string) string {
	if diag.Analyzer.Name == linterName {
		return diag.Message
	}
	return fmt.Sprintf("%s: %s", diag.Analyzer.Name, diag.Message)
}
