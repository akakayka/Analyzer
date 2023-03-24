using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Threading;

namespace Analyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class VarAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "Analyzer";

        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.VariableDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeForeachNode, SyntaxKind.ForEachStatement);
        }

        private void AnalyzeForeachNode(SyntaxNodeAnalysisContext context)
        {
            var foreachStatement = (ForEachStatementSyntax)context.Node;
            
            if (foreachStatement.Type.IsVar)
            {
                return;
            }
            context.ReportDiagnostic(Diagnostic.Create
                (Rule, context.Node.ChildNodes().Where(x => x.GetType() == typeof(PredefinedTypeSyntax)).First().GetLocation(), foreachStatement.Identifier.ValueText));
        }
        private void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var variableDeclaration = (VariableDeclarationSyntax)context.Node;
            if ((variableDeclaration.Parent.GetType() != typeof(LocalDeclarationStatementSyntax)
                && variableDeclaration.Parent.GetType() != typeof(ForStatementSyntax)) || variableDeclaration.Type.IsVar)
            {
                return;
            }
            if (!variableDeclaration.ToString().Contains('='))
            {
                return;
            }
            context.ReportDiagnostic(Diagnostic.Create
                (Rule, context.Node.ChildNodes().Where(x => x.GetType() == typeof(PredefinedTypeSyntax) || x.GetType() == typeof(GenericNameSyntax) || x.GetType() == typeof(IdentifierNameSyntax)).First().GetLocation(), variableDeclaration.Type.ToString()));
        }
    }
}
