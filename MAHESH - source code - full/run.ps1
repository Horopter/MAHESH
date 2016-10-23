$t = '[DllImport("user32.dll")] public static extern bool ShowWindow(int handle, int state);'
add-type -name win -member $t -namespace native
[native.win]::ShowWindow(([System.Diagnostics.Process]::GetCurrentProcess() | Get-Process).MainWindowHandle, 0)
& C:\"Program Files\R\R-3.3.0\bin\x64\Rscript.exe" --vanilla "C:\Users\Horopter\Documents\mahesh\run.R"