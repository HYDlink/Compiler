using System.Diagnostics;

namespace SyntaxAnalyzer;

public static class Utilities
{
    public static void ExportDotToSvg(this string dot, string name, string format)
    {
        var dot_file = $"{name}.dot";
        var bat_file = $"{name}.bat";
        var output_file = $"{name}.{format}";
        File.WriteAllText(dot_file, dot);


        var bat =
            $"dot -T{format} {dot_file} -o {output_file}";


        File.WriteAllText(bat_file, bat);
        var result = Process.Start(bat_file);
        result.WaitForExit();


        // File.Delete(dot_file);
        File.Delete(bat_file);
        OpenFile(output_file);
    }

    public static void OpenFile(string filename)
    {
        var start_info = new ProcessStartInfo(filename)
        { UseShellExecute = true };
        Process.Start(start_info);
    }
}