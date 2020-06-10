package goanalysis

import (
	"fmt"
	"go/token"
	"reflect"
	"testing"

	"github.com/golangci/golangci-lint/pkg/result"
	"golang.org/x/tools/go/analysis"

	"github.com/stretchr/testify/assert"
	"golang.org/x/tools/go/packages"
)

func TestParseError(t *testing.T) {
	cases := []struct {
		in, out string
		good    bool
	}{
		{"f.go:1:2", "", true},
		{"f.go:1", "", true},
		{"f.go", "", false},
		{"f.go: 1", "", false},
	}

	for _, c := range cases {
		i, _ := parseError(packages.Error{
			Pos: c.in,
			Msg: "msg",
		})
		if !c.good {
			assert.Nil(t, i)
			continue
		}

		assert.NotNil(t, i)

		pos := fmt.Sprintf("%s:%d", i.FilePath(), i.Line())
		if i.Pos.Column != 0 {
			pos += fmt.Sprintf(":%d", i.Pos.Column)
		}
		out := pos
		expOut := c.out
		if expOut == "" {
			expOut = c.in
		}
		assert.Equal(t, expOut, out)

		assert.Equal(t, "typecheck", i.FromLinter)
		assert.Equal(t, "msg", i.Text)
	}
}

func Test_buildIssues(t *testing.T) {
	type args struct {
		diags             []Diagnostic
		linterNameBuilder func(diag *Diagnostic) string
	}
	tests := []struct {
		name string
		args args
		want []result.Issue
	}{
		{
			name: "No Diagnostics",
			args: args{
				diags: []Diagnostic{},
				linterNameBuilder: func(*Diagnostic) string {
					return "some-linter"
				},
			},
			want: []result.Issue(nil),
		},
		{
			name: "Linter Name is Analyzer Name",
			args: args{
				diags: []Diagnostic{
					{
						Diagnostic: analysis.Diagnostic{
							Message: "failure message",
						},
						Analyzer: &analysis.Analyzer{
							Name: "some-linter",
						},
						Position: token.Position{},
						Pkg:      nil,
					},
				},
				linterNameBuilder: func(*Diagnostic) string {
					return "some-linter"
				},
			},
			want: []result.Issue{
				{
					FromLinter: "some-linter",
					Text:       "failure message",
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := buildIssues(tt.args.diags, tt.args.linterNameBuilder); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("buildIssues() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_buildSingleIssue(t *testing.T) {
	type args struct {
		diag       *Diagnostic
		linterName string
	}
	fakePkg := packages.Package{
		Fset: makeFakeFileSet(),
	}
	tests := []struct {
		name      string
		args      args
		wantIssue result.Issue
	}{
		{
			name: "Linter Name is Analyzer Name",
			args: args{
				diag: &Diagnostic{
					Diagnostic: analysis.Diagnostic{
						Message: "failure message",
					},
					Analyzer: &analysis.Analyzer{
						Name: "some-linter",
					},
					Position: token.Position{},
					Pkg:      nil,
				},

				linterName: "some-linter",
			},
			wantIssue: result.Issue{
				FromLinter: "some-linter",
				Text:       "failure message",
			},
		},
		{
			name: "Linter Name is NOT Analyzer Name",
			args: args{
				diag: &Diagnostic{
					Diagnostic: analysis.Diagnostic{
						Message: "failure message",
					},
					Analyzer: &analysis.Analyzer{
						Name: "some-analyzer",
					},
					Position: token.Position{},
					Pkg:      nil,
				},
				linterName: "some-linter",
			},
			wantIssue: result.Issue{
				FromLinter: "some-linter",
				Text:       "some-analyzer: failure message",
			},
		},
		{
			name: "Shows issue when suggested edits exist but has no TextEdits",
			args: args{
				diag: &Diagnostic{
					Diagnostic: analysis.Diagnostic{
						Message: "failure message",
						SuggestedFixes: []analysis.SuggestedFix{
							{
								Message:   "fix something",
								TextEdits: []analysis.TextEdit{},
							},
						},
					},
					Analyzer: &analysis.Analyzer{
						Name: "some-analyzer",
					},
					Position: token.Position{},
					Pkg:      nil,
				},
				linterName: "some-linter",
			},
			wantIssue: result.Issue{
				FromLinter: "some-linter",
				Text:       "some-analyzer: failure message",
			},
		},
		{
			name: "Replace Whole Line",
			args: args{
				diag: &Diagnostic{
					Diagnostic: analysis.Diagnostic{
						Message: "failure message",
						SuggestedFixes: []analysis.SuggestedFix{
							{
								Message: "fix something",
								TextEdits: []analysis.TextEdit{
									{
										Pos:     101,
										End:     201,
										NewText: []byte("// Some comment to fix\n"),
									},
								},
							},
						},
					},
					Analyzer: &analysis.Analyzer{
						Name: "some-analyzer",
					},
					Position: token.Position{},
					Pkg:      &fakePkg,
				},
				linterName: "some-linter",
			},
			wantIssue: result.Issue{
				FromLinter: "some-linter",
				Text:       "some-analyzer: failure message",
				LineRange: &result.Range{
					From: 2,
					To:   2,
				},
				Replacement: &result.Replacement{
					NeedOnlyDelete: false,
					NewLines: []string{
						"// Some comment to fix",
					},
				},
				Pkg: &fakePkg,
			},
		},
		{
			name: "Excludes Replacement if TextEdit doesn't modify only whole lines",
			args: args{
				diag: &Diagnostic{
					Diagnostic: analysis.Diagnostic{
						Message: "failure message",
						SuggestedFixes: []analysis.SuggestedFix{
							{
								Message: "fix something",
								TextEdits: []analysis.TextEdit{
									{
										Pos:     101,
										End:     151,
										NewText: []byte("// Some comment to fix\n"),
									},
								},
							},
						},
					},
					Analyzer: &analysis.Analyzer{
						Name: "some-analyzer",
					},
					Position: token.Position{},
					Pkg:      &fakePkg,
				},
				linterName: "some-linter",
			},
			wantIssue: result.Issue{
				FromLinter: "some-linter",
				Text:       "some-analyzer: failure message",
				Pkg:        &fakePkg,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if gotIssues := buildSingleIssue(tt.args.diag, tt.args.linterName); !reflect.DeepEqual(gotIssues, tt.wantIssue) {
				t.Errorf("buildSingleIssue() = %v, want %v", gotIssues, tt.wantIssue)
			}
		})
	}
}

func makeFakeFileSet() *token.FileSet {
	fSet := token.NewFileSet()
	file := fSet.AddFile("fake.go", 1, 1000)
	for i := 100; i < 1000; i += 100 {
		file.AddLine(i)
	}
	return fSet
}
