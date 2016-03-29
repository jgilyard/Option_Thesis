Attribute VB_Name = "Module1"
Dim Bloomberg_Add As String
Dim Num_Rows As Integer

Sub L_P_Puller(Current_Stock As String)
Attribute L_P_Puller.VB_ProcData.VB_Invoke_Func = " \n14"

Bloomberg_Add = "=BDH(" + Chr(34) + Current_Stock + Chr(34) + "," + Chr(34) + "PX_LAST" + Chr(34) + "," + Chr(34) + "1/1/2005" + Chr(34) + "," + Chr(34) + "12/31/2016" + Chr(34) + "," + Chr(34) + "Dir=V" + Chr(34) + "," + Chr(34) + "Dts=S" + Chr(34) + "," + Chr(34) + "Sort=A" + Chr(34) + "," + Chr(34) + "Quote=C" + Chr(34) + "," + Chr(34) + "QtTyp=Y" + Chr(34) + "," + Chr(34) + "Days=T" + Chr(34) + "," + Chr(34) + "Per=cd" + Chr(34) + "," + Chr(34) + "DtFmt=D" + Chr(34) + "," + Chr(34) + "UseDPDF=Y" + Chr(34) + "," + Chr(34) + "cols=2;rows=2793" + Chr(34) + ")"

End Sub
Sub Num_of_Indexs(Current_Sheet As Worksheet)
Num_Rows = Current_Sheet.Range("A1048576").End(xlUp).Row
End Sub


Sub Main()
'Equity List as Worksheet
Dim EL As Worksheet
'Pulled Prices as Worksheet
Dim PP As Worksheet
Set EL = Sheets("Equity_List")
Set PP = Sheets("Pulled_Prices")
Call Num_of_Indexs(EL)
Dim Num_Pulls As Integer

Dim index As Integer
    For index = 1 To Num_Rows
    Dim Func As String
    Call L_P_Puller(EL.Cells(index + 1, 3))
    PP.Cells(3, 1 + (index - 1) * 2) = (EL.Cells(index + 1, 3))
    PP.Cells(5, 1 + ((index - 1) * 2)) = Bloomberg_Add
Next

End Sub

Sub Del_Col()
Num_Rows = Num_Rows - 1
Dim cur As Worksheet
Set cur = Sheets("Equity_List")
Dim PP As Worksheet
Set PP = Sheets("Pulled_Prices")
Call Num_of_Indexs(cur)
MsgBox (Num_Rows)
Dim i As Integer
For i = 1 To (Num_Rows * 2) Step 2
    If Not PP.Cells(7, i).Text = "1/5/2005" Then
    PP.Columns(i + 1).EntireColumn.Delete
    PP.Columns(i).EntireColumn.Delete
    End If
Next i
End Sub

Sub Copy_EQ_Name()
Num_Rows = Num_Rows - 1
Dim cur As Worksheet
Set cur = Sheets("Equity_List")
Dim PP As Worksheet
Set PP = Sheets("Pulled_Prices")
Call Num_of_Indexs(cur)
Dim i As Integer

For i = 1 To (Num_Rows * 2) Step 2

Next i

End Sub
