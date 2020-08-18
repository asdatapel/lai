#include <fstream>
#include <string>
#include <iostream>

void write_to_file(std::string filename, std::string contents)
{
    std::ofstream out(filename);
    out << contents;
    out.close();
}