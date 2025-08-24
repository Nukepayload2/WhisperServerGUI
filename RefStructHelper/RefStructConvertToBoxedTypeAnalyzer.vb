Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class RefStructConvertToBoxedTypeAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "BCX31394"

    ' You can change these strings in the Resources.resx file.
    Private Shared ReadOnly Title As LocalizableString = "Restricted type cannot be converted to Object"
    Private Shared ReadOnly MessageFormat As LocalizableString = "Expression of type '{0}' cannot be converted to 'Object' or 'ValueType'"
    Private Shared ReadOnly Description As LocalizableString = "Restricted types cannot be converted to Object or ValueType."
    Private Const Category As String = "Type Safety"

    Private Shared ReadOnly Rule As New DiagnosticDescriptor(
        DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault:=True, description:=Description)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) = ImmutableArray.Create(Rule)

    Public Overrides Sub Initialize(context As AnalysisContext)
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None)
        context.EnableConcurrentExecution()

        ' Register for compilation start action
        context.RegisterCompilationStartAction(AddressOf CompilationStartAction)
    End Sub

    Private Sub CompilationStartAction(context As CompilationStartAnalysisContext)
        ' Check if OptionRestrict is enabled
        Dim compilation = context.Compilation
        If Not IsOptionRestrictEnabled(compilation) Then
            Return
        End If

        ' Create a cache for restricted type checks to improve performance
        Dim restrictedTypeCache As New ConcurrentDictionary(Of ITypeSymbol, Boolean)(SymbolEqualityComparer.Default)

        ' Register for syntax node actions to catch various conversion scenarios
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeCastExpression(ctx, restrictedTypeCache), SyntaxKind.CTypeExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeCastExpression(ctx, restrictedTypeCache), SyntaxKind.DirectCastExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeCastExpression(ctx, restrictedTypeCache), SyntaxKind.TryCastExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeAssignment(ctx, restrictedTypeCache), SyntaxKind.SimpleAssignmentStatement)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeArgument(ctx, restrictedTypeCache), SyntaxKind.SimpleArgument)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeArrayCreation(ctx, restrictedTypeCache), SyntaxKind.ArrayCreationExpression)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeReturnStatement(ctx, restrictedTypeCache), SyntaxKind.ReturnStatement)
        context.RegisterSyntaxNodeAction(Sub(ctx) AnalyzeLocalDeclaration(ctx, restrictedTypeCache), SyntaxKind.LocalDeclarationStatement)
    End Sub

    Private Function IsOptionRestrictEnabled(compilation As Compilation) As Boolean
        ' Check if <OptionRestrict>On</OptionRestrict> is set in the project
        ' Look for the specific compilation option
        Dim visualBasicCompilation = TryCast(compilation, VisualBasicCompilation)
        If visualBasicCompilation IsNot Nothing Then
            ' This would require accessing the compilation options
            Return True
        End If
        Return True ' For now, assume it's enabled for testing
    End Function

    Private Sub AnalyzeCastExpression(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim castNode = DirectCast(context.Node, CastExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the expression being cast
        Dim expressionType = semanticModel.GetTypeInfo(castNode.Expression, cancellationToken).Type
        Dim targetType = semanticModel.GetTypeInfo(castNode.Type, cancellationToken).Type

        ' Check for restricted type conversion to Object or ValueType
        CheckRestrictedConversion(expressionType, targetType, castNode, context, restrictedTypeCache)
    End Sub

    Private Sub AnalyzeAssignment(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim assignmentNode = DirectCast(context.Node, AssignmentStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the expression being assigned
        Dim expressionType = semanticModel.GetTypeInfo(assignmentNode.Right, cancellationToken).Type
        Dim targetType = semanticModel.GetTypeInfo(assignmentNode.Left, cancellationToken).Type

        ' Check for restricted type conversion to Object or ValueType
        CheckRestrictedConversion(expressionType, targetType, assignmentNode, context, restrictedTypeCache)
    End Sub

    Private Sub AnalyzeArgument(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim argNode = DirectCast(context.Node, SimpleArgumentSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the type of the argument expression
        Dim expressionType = semanticModel.GetTypeInfo(argNode.Expression, cancellationToken).Type

        ' Try to determine the target parameter type
        Dim targetType As ITypeSymbol = Nothing
        Dim parameter = GetParameterForArgument(argNode, semanticModel, cancellationToken)
        If parameter IsNot Nothing Then
            targetType = parameter.Type
        End If

        ' Check for restricted type conversion to Object or ValueType
        If targetType IsNot Nothing Then
            CheckRestrictedConversion(expressionType, targetType, argNode, context, restrictedTypeCache)
        End If
    End Sub

    Private Sub AnalyzeArrayCreation(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim arrayNode = DirectCast(context.Node, ArrayCreationExpressionSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Get the array type
        Dim arrayType = semanticModel.GetTypeInfo(arrayNode, cancellationToken).Type
        If arrayType Is Nothing OrElse arrayType.Kind <> SymbolKind.ArrayType Then
            Return
        End If

        Dim elementType = DirectCast(arrayType, IArrayTypeSymbol).ElementType

        ' Check if the element type is Object or ValueType
        If IsObjectType(elementType) OrElse IsValueTypeType(elementType) Then
            ' Check if any initializer expressions are restricted types
            If arrayNode.Initializer IsNot Nothing Then
                For Each expr In arrayNode.Initializer.Initializers
                    Dim exprType As ITypeSymbol = semanticModel.GetTypeInfo(expr, cancellationToken).Type
                    If exprType IsNot Nothing AndAlso IsRestrictedType(exprType, restrictedTypeCache) Then
                        Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, expr.GetLocation(), exprType.Name)
                        context.ReportDiagnostic(diagnostic)
                    End If
                Next
            End If
        End If
    End Sub

    Private Sub AnalyzeReturnStatement(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim returnNode = DirectCast(context.Node, ReturnStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        If returnNode.Expression Is Nothing Then
            Return
        End If

        ' Get the return expression type
        Dim expressionType = semanticModel.GetTypeInfo(returnNode.Expression, cancellationToken).Type

        ' Try to get the return type of the containing method
        Dim containingSymbol = semanticModel.GetEnclosingSymbol(returnNode.SpanStart, cancellationToken)
        Dim methodSymbol As IMethodSymbol = TryCast(containingSymbol, IMethodSymbol)
        If methodSymbol Is Nothing Then
            methodSymbol = TryCast(containingSymbol.ContainingSymbol, IMethodSymbol)
        End If

        If methodSymbol IsNot Nothing Then
            Dim returnType = methodSymbol.ReturnType
            ' Check for restricted type conversion to Object or ValueType
            CheckRestrictedConversion(expressionType, returnType, returnNode.Expression, context, restrictedTypeCache)
        End If
    End Sub

    Private Sub AnalyzeLocalDeclaration(context As SyntaxNodeAnalysisContext, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean))
        Dim localDeclNode = DirectCast(context.Node, LocalDeclarationStatementSyntax)
        Dim semanticModel = context.SemanticModel
        Dim cancellationToken = context.CancellationToken

        ' Check each declarator in the declaration
        For Each declarator In localDeclNode.Declarators
            ' Check if there's an initializer
            If declarator.Initializer IsNot Nothing Then
                ' Get the type of the variable being declared
                Dim variableType As ITypeSymbol = Nothing
                If declarator.AsClause IsNot Nothing Then
                    variableType = semanticModel.GetTypeInfo(declarator.AsClause.Type, cancellationToken).Type
                Else
                    ' Infer the type from the initializer
                    variableType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type
                End If

                ' Get the type of the initializer expression
                Dim initializerType = semanticModel.GetTypeInfo(declarator.Initializer.Value, cancellationToken).Type

                ' Check for restricted type conversion to Object or ValueType
                CheckRestrictedConversion(initializerType, variableType, declarator.Initializer.Value, context, restrictedTypeCache)
            End If
        Next
    End Sub

    Private Sub CheckRestrictedConversion(expressionType As ITypeSymbol,
                                          targetType As ITypeSymbol,
                                          node As SyntaxNode,
                                          context As SyntaxNodeAnalysisContext,
                                          restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean),
                                          <CallerMemberName> Optional callerName As String = Nothing)
        ' Check if this is a restricted type being converted to Object or ValueType
        If expressionType IsNot Nothing AndAlso targetType IsNot Nothing Then
            If IsRestrictedType(expressionType, restrictedTypeCache) AndAlso (IsObjectType(targetType) OrElse IsValueTypeType(targetType)) Then
                Dim diagnostic As Diagnostic = Diagnostic.Create(Rule, node.GetLocation(), expressionType.Name)
                context.ReportDiagnostic(diagnostic)
            End If
        End If
    End Sub

    Private Function GetParameterForArgument(argNode As SimpleArgumentSyntax, semanticModel As SemanticModel, cancellationToken As CancellationToken) As IParameterSymbol
        ' Try to determine which parameter this argument corresponds to
        ' This requires walking up the syntax tree to find the invocation or object creation

        Dim parent = argNode.Parent
        While parent IsNot Nothing
            If TypeOf parent Is InvocationExpressionSyntax Then
                Dim invocation = DirectCast(parent, InvocationExpressionSyntax)
                Dim symbolInfo = semanticModel.GetSymbolInfo(invocation, cancellationToken)
                If symbolInfo.Symbol IsNot Nothing Then
                    Dim methodSymbol = TryCast(symbolInfo.Symbol, IMethodSymbol)
                    If methodSymbol IsNot Nothing Then
                        ' Find the argument index
                        If invocation.ArgumentList Is Nothing Then
                            parent = parent.Parent
                            Continue While
                        End If
                        Dim args = invocation.ArgumentList.Arguments
                        Dim argIndex As Integer = -1
                        For i As Integer = 0 To args.Count - 1
                            If args(i) Is argNode Then
                                argIndex = i
                                Exit For
                            End If
                        Next

                        If argIndex >= 0 AndAlso argIndex < methodSymbol.Parameters.Length Then
                            Return methodSymbol.Parameters(argIndex)
                        End If
                    End If
                End If
            ElseIf TypeOf parent Is ObjectCreationExpressionSyntax Then
                Dim creation = DirectCast(parent, ObjectCreationExpressionSyntax)
                Dim symbolInfo = semanticModel.GetSymbolInfo(creation, cancellationToken)
                If symbolInfo.Symbol IsNot Nothing Then
                    Dim methodSymbol = TryCast(symbolInfo.Symbol, IMethodSymbol)
                    If methodSymbol IsNot Nothing Then
                        ' Find the argument index
                        If creation.ArgumentList Is Nothing Then
                            parent = parent.Parent
                            Continue While
                        End If
                        Dim args = creation.ArgumentList.Arguments
                        Dim argIndex As Integer = -1
                        For i As Integer = 0 To args.Count - 1
                            If args(i) Is argNode Then
                                argIndex = i
                                Exit For
                            End If
                        Next

                        If argIndex >= 0 AndAlso argIndex < methodSymbol.Parameters.Length Then
                            Return methodSymbol.Parameters(argIndex)
                        End If
                    End If
                End If
            End If
            parent = parent.Parent
        End While

        Return Nothing
    End Function

    Private Function IsRestrictedType(typeSymbol As ITypeSymbol, restrictedTypeCache As ConcurrentDictionary(Of ITypeSymbol, Boolean)) As Boolean
        If typeSymbol Is Nothing Then Return False

        ' Check cache first to improve performance
        Dim isRestricted As Boolean
        If restrictedTypeCache.TryGetValue(typeSymbol, isRestricted) Then
            Return isRestricted
        End If

        ' Check if the type has System.Runtime.CompilerServices.IsByRefLikeAttribute
        For Each attribute In typeSymbol.GetAttributes()
            If attribute.AttributeClass?.ToDisplayString() = "System.Runtime.CompilerServices.IsByRefLikeAttribute" Then
                restrictedTypeCache.TryAdd(typeSymbol, True)
                Return True
            End If
        Next

        ' Check for nested restricted types in generic arguments
        If TypeOf typeSymbol Is INamedTypeSymbol Then
            Dim namedType = DirectCast(typeSymbol, INamedTypeSymbol)
            If namedType.IsGenericType Then
                For Each arg In namedType.TypeArguments
                    If IsRestrictedType(arg, restrictedTypeCache) Then
                        restrictedTypeCache.TryAdd(typeSymbol, True)
                        Return True
                    End If
                Next
            End If
        End If

        restrictedTypeCache.TryAdd(typeSymbol, False)
        Return False
    End Function

    Private Function IsObjectType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False
        Return typeSymbol.SpecialType = SpecialType.System_Object
    End Function

    Private Function IsValueTypeType(typeSymbol As ITypeSymbol) As Boolean
        If typeSymbol Is Nothing Then Return False
        Return typeSymbol.SpecialType = SpecialType.System_ValueType
    End Function
End Class
