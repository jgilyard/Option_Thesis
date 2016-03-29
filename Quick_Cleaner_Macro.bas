Attribute VB_Name = "Module2"
Dim Num_Rows As Integer
Dim Num_Cols As Integer

Sub Num_of_Indexs(Current_Sheet As Worksheet)
Num_Rows = Current_Sheet.Range("A1048576").End(xlUp).Row
Num_Cols = Current_Sheet.Cells(1, Columns.Count).End(xlToLeft).Column
End Sub

Sub cleaner()
Dim Current As Worksheet
Set Current = Sheets("Sheet1")
Call Num_of_Indexs(Current)
MsgBox (Num_Cols)
For i = 1 To Num_Cols
    For j = 1 To Num_Rows
    'MsgBox (Current.Cells(j, i))
        If (Current.Cells(j, i).Text = "#N/A N/A") Then
        Current.Cells(j, i) = 0
        End If
    Next
Next
End Sub
