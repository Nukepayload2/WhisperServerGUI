Option Strict On

Imports System.Threading

''' <summary>
''' Parallel helper class that provides wrapper methods for parallel operations
''' </summary>
Public Module ParallelHelper
    ''' <summary>
    ''' Executes a function for each element in the source collection asynchronously
    ''' Wrapper for ForEachAsync that converts ValueTask to Task
    ''' </summary>
    ''' <typeparam name="TSource">The type of the elements in the source collection</typeparam>
    ''' <param name="source">The source collection to iterate over</param>
    ''' <param name="body">The function to execute for each element</param>
    ''' <returns>A Task that represents the asynchronous operation</returns>
    Public Async Function ForEachAsync(Of TSource)(
        source As IEnumerable(Of TSource), parallelOptions As ParallelOptions,
        body As Func(Of TSource, CancellationToken, Task)) As Task
        Await Parallel.ForEachAsync(source, parallelOptions, Function(item, cancellationToken) New ValueTask(body(item, cancellationToken)))
    End Function
End Module
