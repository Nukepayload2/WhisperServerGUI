# VB ref struct 装箱分析器
你是一个 Roslyn 分析器编写助手，你的任务是创建一个 VB ref struct 装箱分析器，报告这些诊断错误。当引用它的项目的 PropertyGroup 里面有 `<OptionRestrict>On</OptionRestrict>` 的时候，你需要启用这套装箱分析器。

## 项目目标
扩展受限类型的范围，只要一个 `Structure` 具有 `System.Runtime.CompilerServices.IsByRefLikeAttribute`，那么它就是受限类型。
新规则产生的诊断 ID 为 BCX 和原来的数字，比如 BCX31396。

## 待办任务
首先你应该列一个 todo 列表（用 markdown 的 - [ ] 作为前缀），然后根据 todo 列表来编写代码。每完成一项 todo，你需要用 - [x] 标记完成。

### 当前实现进度

- [x] 创建项目结构和基本配置
- [x] 添加 Roslyn 分析器依赖包引用
- [x] 实现基本的 RefStructConvertToBoxedTypeAnalyzer 分析器类
- [x] 实现诊断规则 BCX31394 (受限类型不能转换为 Object 或者 ValueType)
- [x] 实现受限类型检测逻辑 (IsByRefLikeAttribute 检查)
- [x] 添加性能优化 (使用缓存避免重复检查)
- [x] 配置分析器支持并发执行和生成代码分析
- [x] 添加诊断规则定义和描述
- [x] 初始化 OptionRestrict 选项检查逻辑框架

### 待完成任务

- [ ] 完善 BCX31396 并拆分诊断规则类 (受限类型不能转换为 `Nullable(Of T)`，并且不能是：数组元素、非受限类型结构体的字段或属性、类的字段或属性、匿名类型成员、ByRef 参数、返回值)
- [ ] 实现诊断规则 BCX32061 (是否允许受限类型作为泛型参数)
- [ ] 实现诊断规则 BCX36598 (防止 LINQ 装箱受限类型)
- [ ] 实现诊断规则 BCX36640 (防止 Lambda 闭包装箱受限类型)
- [ ] 实现诊断规则 BCX37052 (编译为 Async/Iterator 状态机的函数，不允许受限类型变量)
- [ ] 实现诊断规则 BCX31393 (防止调用受限类型继承的实例方法装箱受限类型)
- [ ] 完善 OptionRestrict 选项检查逻辑 (从 MSBuild 编译属性中读取)
- [ ] 添加单元测试项目和测试用例
- [ ] 完善诊断消息的本地化支持
- [ ] 优化性能和内存使用
- [ ] 添加更多边界情况的处理
- [ ] 移动到独立的 GitHub 仓库

## 知识

## 什么是受限类型
受限类型只能在栈分配（无法装箱），而且也无法嵌套 ByRef。因此具有如下限制：

```xml
  <data name="ERR_RestrictedType1" xml:space="preserve">
    <value>BC31396: '{0}' cannot be made nullable, and cannot be used as the data type of an array element, field, anonymous type member, type argument, 'ByRef' parameter, or return statement.</value>
  </data>
  <data name="ERR_ConstraintIsRestrictedType1" xml:space="preserve">
    <value>BC32061: '{0}' cannot be used as a type constraint.</value>
  </data>
  <data name="ERR_CannotLiftRestrictedTypeQuery" xml:space="preserve">
    <value>BC36598: Instance of restricted type '{0}' cannot be used in a query expression.</value>
  </data>
  <data name="ERR_CannotLiftRestrictedTypeLambda" xml:space="preserve">
    <value>BC36640: Instance of restricted type '{0}' cannot be used in a lambda expression.</value>
  </data>
  <data name="ERR_CannotLiftRestrictedTypeResumable1" xml:space="preserve">
    <value>BC37052: Variable of restricted type '{0}' cannot be declared in an Async or Iterator method.</value>
  </data>
  <data name="ERR_RestrictedAccess" xml:space="preserve">
    <value>BC31393: Expression has the type '{0}' which is a restricted type and cannot be used to access members inherited from 'Object' or 'ValueType'.</value>
  </data>
  <data name="ERR_RestrictedConversion1" xml:space="preserve">
    <value>BC31394: Expression of type '{0}' cannot be converted to 'Object' or 'ValueType'.</value>
  </data>
```

## BC36640,BC36598 的实现片段

```vb
Private Sub VerifyCaptured(variableOrParameter As Symbol, syntax As SyntaxNode)
    Dim type As TypeSymbol
    Dim asParameter = TryCast(variableOrParameter, ParameterSymbol)

    If asParameter IsNot Nothing Then
        type = asParameter.Type
    Else
        Dim asVariable = DirectCast(variableOrParameter, LocalSymbol)
        type = asVariable.Type

        If asVariable.IsByRef Then
            Throw ExceptionUtilities.UnexpectedValue(asVariable.IsByRef)
        End If
    End If

    If type.IsRestrictedType Then
        If Binder.IsTopMostEnclosingLambdaAQueryLambda(_currentParent, variableOrParameter.ContainingSymbol) Then
            _diagnostics.Add(ERRID.ERR_CannotLiftRestrictedTypeQuery, syntax.GetLocation(), type)
        Else
            _diagnostics.Add(ERRID.ERR_CannotLiftRestrictedTypeLambda, syntax.GetLocation(), type)
        End If
    End If
End Sub
```

## BC37052 的实现片段
```vb
' Returns deterministically ordered list of variables that ought to be hoisted.
Public Overloads Shared Function Analyze(info As FlowAnalysisInfo, diagnostics As DiagnosticBag) As Result

    If lazyDisallowedCaptures IsNot Nothing Then
        For Each variable In lazyDisallowedCaptures.Keys
            Dim type As TypeSymbol = If(variable.Kind = SymbolKind.Local, TryCast(variable, LocalSymbol).Type, TryCast(variable, ParameterSymbol).Type)
            For Each node In lazyDisallowedCaptures(variable)
                ' Variable of restricted type '{0}' cannot be declared in an Async or Iterator method.
                diagnostics.Add(ERRID.ERR_CannotLiftRestrictedTypeResumable1, node.GetLocation(), type)
            Next
        Next
    End If
```

## BC31394 的实现片段
注意看 ERR_RestrictedConversion1

```vb
Private Function ApplyTryCastConversion(
     node As SyntaxNode,
     argument As BoundExpression,
     targetType As TypeSymbol,
     diagnostics As BindingDiagnosticBag
) As BoundExpression
    Debug.Assert(argument.IsValue)

    ' Deal with erroneous arguments
    If (argument.HasErrors OrElse targetType.IsErrorType) Then
        argument = MakeRValue(argument, diagnostics)

        Return New BoundTryCast(node, argument, conversionKind:=Nothing, type:=targetType, hasErrors:=True)
    End If

    ' Classify conversion
    Dim conv As ConversionKind

    If targetType.IsReferenceType Then
        Dim useSiteInfo = GetNewCompoundUseSiteInfo(diagnostics)
        conv = Conversions.ClassifyTryCastConversion(argument, targetType, Me, useSiteInfo)

        If diagnostics.Add(node, useSiteInfo) Then
            ' Suppress any additional diagnostics
            diagnostics = BindingDiagnosticBag.Discarded
        End If
    Else
        conv = Nothing
    End If

    If ReclassifyExpression(argument, SyntaxKind.TryCastKeyword, node, conv, True, targetType, diagnostics) Then
        If argument.Syntax IsNot node Then
            ' If its an explicit conversion, we must have a bound node that corresponds to that syntax node for GetSemanticInfo.
            ' Insert an identity conversion if necessary.
            Debug.Assert(argument.Kind <> BoundKind.TryCast, "Associated wrong node with conversion?")
            argument = New BoundTryCast(node, argument, ConversionKind.Identity, targetType)
        End If

        Return argument
    Else
        argument = MakeRValue(argument, diagnostics)
    End If

    If argument.HasErrors Then
        Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)
    End If

    Dim sourceType = argument.Type

    If sourceType IsNot Nothing AndAlso sourceType.IsErrorType() Then
        Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)
    End If

    Debug.Assert(argument.IsNothingLiteral() OrElse (sourceType IsNot Nothing AndAlso Not sourceType.IsErrorType()))

    ' Check for special error conditions
    If Conversions.NoConversion(conv) Then
        If targetType.IsValueType() Then
            Dim castSyntax = TryCast(node, CastExpressionSyntax)
            ReportDiagnostic(diagnostics, If(castSyntax IsNot Nothing, castSyntax.Type, node), ERRID.ERR_TryCastOfValueType1, targetType)
            Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)

        ElseIf targetType.IsTypeParameter() AndAlso Not targetType.IsReferenceType Then
            Dim castSyntax = TryCast(node, CastExpressionSyntax)
            ReportDiagnostic(diagnostics, If(castSyntax IsNot Nothing, castSyntax.Type, node), ERRID.ERR_TryCastOfUnconstrainedTypeParam1, targetType)
            Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)

        ElseIf sourceType.IsValueType AndAlso sourceType.IsRestrictedType() AndAlso
               (targetType.IsObjectType() OrElse targetType.SpecialType = SpecialType.System_ValueType) Then
            ReportDiagnostic(diagnostics, argument.Syntax, ERRID.ERR_RestrictedConversion1, sourceType)
            Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)
        End If
    End If

    WarnOnNarrowingConversionBetweenSealedClassAndAnInterface(conv, argument.Syntax, sourceType, targetType, diagnostics)

    If Conversions.NoConversion(conv) Then
        ReportNoConversionError(argument.Syntax, sourceType, targetType, diagnostics)
        Return New BoundTryCast(node, argument, conv, targetType, hasErrors:=True)
    End If

    Dim constantResult = Conversions.TryFoldNothingReferenceConversion(argument, conv, targetType)

    If Not Conversions.IsIdentityConversion(conv) Then
        WarnOnLockConversion(sourceType, argument.Syntax, diagnostics)
    End If

    Return New BoundTryCast(node, argument, conv, constantResult, targetType)
End Function
```

## BC31396 的实现片段
注意看 Restricted 相关的内容

```vb
Dim newParam As ParameterSymbol

If isFromLambda Then
    newParam = UnboundLambdaParameterSymbol.CreateFromSyntax(paramSyntax, name, flags, ordinal, Me, diagBag)
Else
    newParam = SourceComplexParameterSymbol.CreateFromSyntax(container, paramSyntax, name, flags, ordinal, Me, checkModifier, diagBag)
End If
ordinal += 1

If newParam.IsByRef AndAlso (modifiers And SourceMemberFlags.Async) = SourceMemberFlags.Async Then
    ReportDiagnostic(diagBag, paramSyntax, ERRID.ERR_BadAsyncByRefParam)

ElseIf newParam.IsByRef AndAlso (modifiers And SourceMemberFlags.Iterator) = SourceMemberFlags.Iterator Then
    ReportDiagnostic(diagBag, paramSyntax, ERRID.ERR_BadIteratorByRefParam)

Else
    Dim paramType As TypeSymbol = newParam.Type

    If paramType IsNot Nothing Then
        If paramType.IsArrayType() Then
            Dim restrictedType As TypeSymbol = Nothing
            If paramType.IsRestrictedArrayType(restrictedType) Then
                ReportDiagnostic(diagBag, paramSyntax.AsClause.Type, ERRID.ERR_RestrictedType1, restrictedType)
            End If
        ElseIf newParam.IsByRef Then
            If paramType.IsRestrictedType Then
                ReportDiagnostic(diagBag, paramSyntax.AsClause.Type, ERRID.ERR_RestrictedType1, paramType)
            End If
        ElseIf (modifiers And (SourceMemberFlags.Async Or SourceMemberFlags.Iterator)) <> 0 Then
            If paramType.IsRestrictedType Then
                ReportDiagnostic(diagBag, paramSyntax.AsClause.Type, ERRID.ERR_RestrictedResumableType1, paramType)
            End If
        End If
    End If
End If
```

```vb
Dim restrictedType As TypeSymbol = Nothing
If type.IsRestrictedArrayType(restrictedType) Then
    If Not isInitializedByAsNew OrElse Not skipAsNewInitializer Then
        ReportDiagnostic(diagnostics, errSyntax, ERRID.ERR_RestrictedType1, restrictedType)
    End If
ElseIf symbol.IsStatic Then
    If type.IsRestrictedType() Then
        If Not isInitializedByAsNew OrElse Not skipAsNewInitializer Then
            ReportDiagnostic(diagnostics, errSyntax, ERRID.ERR_RestrictedType1, type)
        End If
    ElseIf IsInAsyncContext() OrElse IsInIteratorContext() Then
        ReportDiagnostic(diagnostics, name, ERRID.ERR_BadStaticInitializerInResumable)
    End If
ElseIf IsInAsyncContext() OrElse IsInIteratorContext() Then
    If type.IsRestrictedType() Then
        If Not isInitializedByAsNew OrElse Not skipAsNewInitializer Then
            ReportDiagnostic(diagnostics, errSyntax, ERRID.ERR_CannotLiftRestrictedTypeResumable1, type)
        End If
    End If
End If
```

```vb
Private Sub ReportArrayLiteralInferredTypeDiagnostics(arrayLiteral As BoundArrayLiteral, diagnostics As BindingDiagnosticBag)
    Dim targetElementType = arrayLiteral.InferredType.ElementType

    If targetElementType.IsRestrictedType Then
        ReportDiagnostic(diagnostics, arrayLiteral.Syntax, ERRID.ERR_RestrictedType1, targetElementType)
    End If

End Sub
```

```vb
Private Function BindAnonymousObjectCreationExpression(node As VisualBasicSyntaxNode,
                                                       typeDescr As AnonymousTypeDescriptor,
                                                       initExpressions As ImmutableArray(Of BoundExpression),
                                                       diagnostics As BindingDiagnosticBag) As BoundExpression
    '  Check for restricted types.
    For Each field As AnonymousTypeField In typeDescr.Fields
        Dim restrictedType As TypeSymbol = Nothing
        If field.Type.IsRestrictedTypeOrArrayType(restrictedType) Then
            ReportDiagnostic(diagnostics, field.Location, ERRID.ERR_RestrictedType1, restrictedType)
        End If
    Next

    Return CreateAnonymousObjectCreationExpression(node, typeDescr, initExpressions)
End Function
```

```vb
If delegateInvoke.OriginalDefinition.ReturnType.IsTypeParameter() Then
    Dim restrictedType As TypeSymbol = Nothing
    If delegateReturnType.IsRestrictedTypeOrArrayType(restrictedType) Then
        Dim location As SyntaxNode

        If lambda.Expression.Kind = BoundKind.RangeVariableAssignment Then
            location = DirectCast(lambda.Expression, BoundRangeVariableAssignment).Value.Syntax
        Else
            location = lambda.Expression.Syntax
        End If

        ReportDiagnostic(diagnostics, location, ERRID.ERR_RestrictedType1, restrictedType)
    End If
End If
```

```vb
For Each delegateParam As ParameterSymbol In delegateInvoke.Parameters
    If delegateParam.IsByRef OrElse delegateParam.OriginalDefinition.Type.IsTypeParameter() Then
        Dim restrictedType As TypeSymbol = Nothing
        If delegateParam.Type.IsRestrictedTypeOrArrayType(restrictedType) Then
            ReportDiagnostic(diagnostics, lambda.LambdaSymbol.Parameters(delegateParam.Ordinal).Locations(0),
                             ERRID.ERR_RestrictedType1, restrictedType)
        End If
    End If
Next
```

```vb
If node.Kind = SyntaxKind.MultiLineFunctionLambdaExpression AndAlso
   node.SubOrFunctionHeader.AsClause IsNot Nothing Then
    returnType = BindTypeSyntax(node.SubOrFunctionHeader.AsClause.Type, diagnostics)

    If returnType.IsRestrictedType() Then
        ReportDiagnostic(diagnostics, node.SubOrFunctionHeader.AsClause.Type, ERRID.ERR_RestrictedType1, returnType)
        hasErrors = True
```

```vb
If delegateRelaxation <> ConversionKind.DelegateRelaxationLevelInvalid Then
    Debug.Assert(targetReturnType Is lambdaSymbol.ReturnType)

    ' Check return type as well, but complain only if we "inherited" it from the target signature. If we got
    ' it from the lambda's signature or inferred it, we already complained about it.
    Dim restrictedType As TypeSymbol = Nothing
    If targetReturnType.IsRestrictedTypeOrArrayType(restrictedType) Then
        delegateRelaxation = ConversionKind.DelegateRelaxationLevelInvalid 'No conversion
        methodConversions = methodConversions Or MethodConversionKind.Error_RestrictedType

        If source.ReturnType Is Nothing AndAlso target.ReturnType.SpecialType <> SpecialType.System_Void Then
            ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_RestrictedType1, restrictedType)
        End If
    End If
End If
```

```vb
Friend Function InferFunctionLambdaReturnType(
    source As UnboundLambda,
    targetParameters As UnboundLambda.TargetSignature
) As KeyValuePair(Of TypeSymbol, ReadOnlyBindingDiagnostic(Of AssemblySymbol))
    Debug.Assert(Me Is source.Binder AndAlso source.IsFunctionLambda AndAlso
                 source.ReturnType Is Nothing AndAlso targetParameters.ReturnType.IsVoidType())

    ' If both Async and Iterator are specified, we cannot really infer return type.
    If source.Flags = (SourceMemberFlags.Async Or SourceMemberFlags.Iterator) Then
        ' No need to report any error because we complained about conflicting modifiers.
        Return New KeyValuePair(Of TypeSymbol, ReadOnlyBindingDiagnostic(Of AssemblySymbol))(LambdaSymbol.ReturnTypeIsUnknown, ReadOnlyBindingDiagnostic(Of AssemblySymbol).Empty)
    End If

    Dim diagnostics = BindingDiagnosticBag.GetInstance(withDiagnostics:=True, source.WithDependencies)

    ' Clone parameters. 
    Dim parameters As ImmutableArray(Of BoundLambdaParameterSymbol) = BuildBoundLambdaParameters(source, targetParameters, diagnostics)

    Dim symbol = New SourceLambdaSymbol(source.Syntax, source, parameters, LambdaSymbol.ReturnTypeIsBeingInferred, Me)
    Dim block As BoundBlock = BindLambdaBody(symbol, diagnostics, lambdaBinder:=Nothing)

    If block.HasErrors OrElse diagnostics.HasAnyErrors() Then
        Return New KeyValuePair(Of TypeSymbol, ReadOnlyBindingDiagnostic(Of AssemblySymbol))(LambdaSymbol.ReturnTypeIsUnknown, diagnostics.ToReadOnlyAndFree())
    End If

    diagnostics.Clear()

    Dim lambdaReturnType As TypeSymbol
    Dim returnExpressions = ArrayBuilder(Of BoundExpression).GetInstance()

    LambdaReturnStatementsVisitor.CollectReturnExpressions(block, returnExpressions, source.Flags = SourceMemberFlags.Iterator)

    If returnExpressions.Count = 0 AndAlso source.Flags = SourceMemberFlags.Async Then
        ' It's fine if there were no return statements in an Async Function.
        ' It simply returns "Task".
        lambdaReturnType = GetWellKnownType(WellKnownType.System_Threading_Tasks_Task, source.Syntax, diagnostics)

    ElseIf returnExpressions.Count = 0 AndAlso source.Flags = SourceMemberFlags.Iterator Then
        ' It's fine if there were no yield statements in an Iterator Function.
        ' It simply returns IEnumerable(of Object).
        lambdaReturnType = GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T, source.Syntax, diagnostics).
            Construct(GetSpecialType(SpecialType.System_Object, source.Syntax, diagnostics))

    Else
        ' Inference is different for Expression and Statement lambdas
        If source.IsSingleLine Then
            Debug.Assert(returnExpressions.Count < 2)

            Dim returnExpression As BoundExpression = Nothing

            If returnExpressions.Count > 0 Then
                returnExpression = MakeRValue(returnExpressions(0), diagnostics)
            End If

            If returnExpression IsNot Nothing AndAlso Not returnExpression.HasErrors AndAlso Not diagnostics.HasAnyErrors() Then
                lambdaReturnType = returnExpression.Type
            Else
                lambdaReturnType = GetSpecialType(SpecialType.System_Object, source.Syntax, diagnostics)
            End If

            diagnostics.Clear()

            Dim restrictedType As TypeSymbol = Nothing
            If lambdaReturnType.IsRestrictedTypeOrArrayType(restrictedType) Then
                ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_RestrictedType1, restrictedType)
            End If
        Else
            Dim numCandidates As Integer = 0
            lambdaReturnType = InferDominantTypeOfExpressions(source.Syntax, returnExpressions, diagnostics, numCandidates)

            Debug.Assert(lambdaReturnType IsNot Nothing OrElse numCandidates = 0)

            Dim restrictedType As TypeSymbol = Nothing
            If lambdaReturnType Is Nothing Then
                ' "Cannot infer a return type. Specifying the return type might correct this error."
                ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_LambdaNoType)
                lambdaReturnType = LambdaSymbol.ReturnTypeIsUnknown

            ElseIf lambdaReturnType.IsRestrictedTypeOrArrayType(restrictedType) Then
                ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_RestrictedType1, restrictedType)

            ElseIf numCandidates <> 1 Then
                If OptionStrict = OptionStrict.On Then
                    If numCandidates = 0 Then
                        ' "Cannot infer a return type, and Option Strict On does not allow 'Object' to be assumed. Specifying the return type might correct this error."
                        ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_LambdaNoTypeObjectDisallowed)
                        Debug.Assert(lambdaReturnType.IsObjectType())
                    Else
                        ' "Cannot infer a return type because more than one type is possible. Specifying the return type might correct this error."
                        ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.ERR_LambdaTooManyTypesObjectDisallowed)
                        Debug.Assert(lambdaReturnType.IsObjectType())
                    End If
                ElseIf OptionStrict = OptionStrict.Custom Then
                    If numCandidates = 0 Then
                        ' "Cannot infer a return type; 'Object' assumed."
                        ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.WRN_ObjectAssumed1, ErrorFactory.ErrorInfo(ERRID.WRN_LambdaNoTypeObjectAssumed))
                        Debug.Assert(lambdaReturnType.IsObjectType())
                    Else
                        ' "Cannot infer a return type because more than one type is possible; 'Object' assumed."
                        ReportDiagnostic(diagnostics, LambdaHeaderErrorNode(source), ERRID.WRN_ObjectAssumed1, ErrorFactory.ErrorInfo(ERRID.WRN_LambdaTooManyTypesObjectAssumed))
                        Debug.Assert(lambdaReturnType.IsObjectType())
                    End If
                End If
            End If
        End If

        If source.Flags = SourceMemberFlags.Async Then
            ' There were some returns with values from Async lambda, infer Task(Of T) as return type of the lambda.
            lambdaReturnType = GetWellKnownType(WellKnownType.System_Threading_Tasks_Task_T, source.Syntax, diagnostics).Construct(lambdaReturnType)

        ElseIf source.Flags = SourceMemberFlags.Iterator Then
            ' There were some returns with values from Iterator lambda, infer IEnumerable(Of T) as return type of the lambda.
            lambdaReturnType = GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T, source.Syntax, diagnostics).Construct(lambdaReturnType)

        End If
    End If

    returnExpressions.Free()

    Return New KeyValuePair(Of TypeSymbol, ReadOnlyBindingDiagnostic(Of AssemblySymbol))(lambdaReturnType, diagnostics.ToReadOnlyAndFree())
End Function
```

```vb
If Conversions.IsDelegateRelaxationSupportedFor(methodConversions) Then
    If Conversions.IsStubRequiredForMethodConversion(methodConversions) Then
        ' We will need a stub for this lambda, which means that we will need to instantiate
        ' an Anonymous Delegate matching the signature of the lambdaSymbol.
        ' Anonymous Delegate has some limitations on parameter and return types.
        For Each param In lambdaSymbol.Parameters
            ' Verify for restricted types.
            Dim restrictedType As TypeSymbol = Nothing
            If param.Type.IsRestrictedTypeOrArrayType(restrictedType) Then
                ReportDiagnostic(diagnostics,
                                 DirectCast(source.Parameters(param.Ordinal), UnboundLambdaParameterSymbol).TypeSyntax,
                                 ERRID.ERR_RestrictedType1, restrictedType)
                delegateRelaxation = ConversionKind.DelegateRelaxationLevelInvalid 'No conversion
                methodConversions = methodConversions Or MethodConversionKind.Error_RestrictedType
                Exit For
            End If
        Next
```

```vb
Private Function BindInitializersAndCreateBoundNode(owningSyntax As VisualBasicSyntaxNode,
                                                    initializerSyntax As ObjectMemberInitializerSyntax,
                                                    diagnostics As BindingDiagnosticBag,
                                                    typeLocationToken As SyntaxToken) As BoundExpression
    Dim fieldsCount As Integer = Me._fields.Length

    ' Try to bind expressions from field initializers one-by-one; after each of the 
    ' expression is bound successfully assign the type of the field in 'fields'.
    Dim boundInitializers(fieldsCount - 1) As BoundExpression

    ' WARNING: Note that SemanticModel.GetDeclaredSymbol for field initializer node relies on 
    '          the fact that the order of properties in anonymous type template corresponds 
    '          1-to-1 to the appropriate filed initializer syntax nodes; This means such 
    '          correspondence must be preserved all the time including erroneous scenarios

    ' NOTE: if one field initializer references another, the binder creates an 
    '       BoundAnonymousTypePropertyAccess node to represent the value of the field, 
    '       if the field referenced is not processed yet an error will be generated
    For index = 0 To fieldsCount - 1
        Dim initializer As FieldInitializerSyntax = initializerSyntax.Initializers(index)

        ' to be used if we need to create BoundAnonymousTypePropertyAccess node
        Dim namedFieldInitializer As NamedFieldInitializerSyntax = Nothing

        Dim initExpression As ExpressionSyntax = Nothing
        If initializer.Kind = SyntaxKind.InferredFieldInitializer Then
            initExpression = DirectCast(initializer, InferredFieldInitializerSyntax).Expression
        Else
            namedFieldInitializer = DirectCast(initializer, NamedFieldInitializerSyntax)
            initExpression = namedFieldInitializer.Expression
        End If

        Dim initializerBinder As New AnonymousTypeFieldInitializerBinder(Me, index)

        Dim boundExpression As BoundExpression = initializerBinder.BindRValue(initExpression, diagnostics)
        boundExpression = New BoundAnonymousTypeFieldInitializer(initializer, initializerBinder, boundExpression, boundExpression.Type).MakeCompilerGenerated()

        boundInitializers(index) = boundExpression

        Dim fieldType As TypeSymbol = boundExpression.Type

        '  check for restricted type
        Dim restrictedType As TypeSymbol = Nothing
        If fieldType.IsRestrictedTypeOrArrayType(restrictedType) Then
            ReportDiagnostic(diagnostics, initExpression, ERRID.ERR_RestrictedType1, restrictedType)
        End If

    Next

End Function
```
