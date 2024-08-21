#include "codewriter.h"

CodeWriter::CodeWriter() {};

CodeWriter::CodeWriter(std::string fileName)
{
    outputFile.open(fileName);
    labelIndex = 1;
}

void CodeWriter::writeArithmetic(std::string command)
{
    writeOutputLine("// " + command);
    if (command == "add" || command == "sub")
    {
        // Pop 2 values from the stack
        writePopCommand();
        writeOutputLine("D=M");
        writeOutputLine("A=A-1");
        // Perform addition/subtraction and save value in current stack slot
        outputFile << "M=" << (command == "add" ? "D+M" : "M-D") << std::endl;
    }
    else if (command == "neg")
    {
        // Pop value from the stack
        writePopCommand();
        // Negate the value
        writeOutputLine("M=-M");
        // Set SP+1
        writeSPStepCommand();
    }
    else if (command == "eq")
    {
        writeEQGTLTCommand("EQ");
    }
    else if (command == "gt")
    {
        writeEQGTLTCommand("GT");
    }
    else if (command == "lt")
    {
        writeEQGTLTCommand("LT");
    }
    else if (command == "and" || command == "or")
    {
        // Pop 2 value from the stack
        writePopCommand();
        writeOutputLine("D=M");
        writePopCommand();
        if (command == "and")
        {
            writeOutputLine("M=D&M");
        }
        else
        {
            writeOutputLine("M=D|M");
        }
        writeSPStepCommand();
    }
    else if (command == "not")
    {
        writePopCommand();
        writeOutputLine("M=!M");
        writeSPStepCommand();
    }
}

void CodeWriter::writePushPop(Parser::CommandType commandType, std::string segment, int index)
{
    writeOutputLine("// " + std::to_string(commandType) + " " + segment + " " + std::to_string(index));
    if (commandType == Parser::C_POP)
    {
        writeOutputLine("@" + getSegmentPointer(segment));
        writeOutputLine(segment == "temp" ? "D=A" : "D=M");
        writeOutputLine("@" + std::to_string(index));
        writeOutputLine("D=D+A");
        writeOutputLine("@SP");
        writeOutputLine("A=M");
        writeOutputLine("M=D");
        writeOutputLine("A=A-1");
        writeOutputLine("D=M");
        writeOutputLine("A=A+1");
        writeOutputLine("A=M");
        writeOutputLine("M=D");
        writeOutputLine("@SP");
        writeOutputLine("M=M-1");
    }
    else if (commandType == Parser::C_PUSH)
    {
        if (segment == "constant")
        {
            writeOutputLine("@" + std::to_string(index));
            writeOutputLine("D=A");
        }
        else
        {
            writeOutputLine("@" + getSegmentPointer(segment));
            writeOutputLine("D=M");
            writeOutputLine("@" + std::to_string(index));
            writeOutputLine("A=D+A");
            writeOutputLine("D=M");
        }
        writeOutputLine("@SP");
        writeOutputLine("A=M");
        writeOutputLine("M=D");
        writeOutputLine("@SP");
        writeOutputLine("M=M+1");
    }
}

void CodeWriter::close()
{
    outputFile.close();
}

void CodeWriter::writePopCommand()
{
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
}

void CodeWriter::writeSPStepCommand()
{
    writeOutputLine("@SP");
    writeOutputLine("M=M+1");
}

void CodeWriter::writeLabel(std::string label)
{
    outputFile << "(" << label << labelIndex << ")" << std::endl;
    labelIndex++;
}

void CodeWriter::writeOutputLine(std::string command)
{
    outputFile << command << std::endl;
}

void CodeWriter::writeEQGTLTCommand(std::string commandLabel)
{
    // Pop 2 value from the stack
    writePopCommand();
    writeOutputLine("D=M");
    writePopCommand();
    writeOutputLine("D=M-D");
    writeOutputLine("M=-1");
    writeOutputLine("@" + commandLabel + std::to_string(labelIndex));
    writeOutputLine("D;J" + commandLabel);
    writeOutputLine("@SP");
    writeOutputLine("A=M");
    writeOutputLine("M=0");
    writeLabel(commandLabel);
    writeSPStepCommand();
}

std::string CodeWriter::getSegmentPointer(std::string segmentLabel)
{
    if (segmentLabel == "local")
    {
        return "LCL";
    }
    else if (segmentLabel == "argument")
    {
        return "ARG";
    }
    else if (segmentLabel == "this")
    {
        return "THIS";
    }
    else if (segmentLabel == "that")
    {
        return "THAT";
    }
    else if (segmentLabel == "temp")
    {
        return std::to_string(5);
    }
    return "";
}