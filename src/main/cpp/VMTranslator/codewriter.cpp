#include "codewriter.h"

CodeWriter::CodeWriter() {};

CodeWriter::CodeWriter(std::string inputPath)
{
    inputPath.erase(inputPath.find_last_of(".") + 1, 2);
    // CodeWriter::inputFileName = getFileName(inputPath);
    outputFile.open(inputPath + "asm");
    labelIndex = 1;
}

void CodeWriter::writeArithmetic(std::string command)
{
    writeOutputLine("// " + command);
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    if (command == "add" || command == "sub")
    {
        // Pop 2 values from the stack
        writeOutputLine("D=M");
        writeOutputLine("A=A-1");
        // Perform addition/subtraction and save value in current stack slot
        outputFile << "M=" << (command == "add" ? "D+M" : "M-D") << std::endl;
    }
    else if (command == "neg")
    {
        writeOutputLine("M=-M");
        writeOutputLine("@SP");
        writeOutputLine("M=M+1");
    }
    else if (command == "eq" || command == "lt" || command == "gt")
    {
        std::transform(command.begin(), command.end(), command.begin(), toupper);
        writeEQGTLTCommand(command);
    }
    else if (command == "and" || command == "or")
    {
        // Pop 2 value from the stack
        writeOutputLine("D=M");
        writeOutputLine("@SP");
        writeOutputLine("AM=M-1");
        if (command == "and")
        {
            writeOutputLine("M=D&M");
        }
        else
        {
            writeOutputLine("M=D|M");
        }
        writeOutputLine("@SP");
        writeOutputLine("M=M+1");
    }
    else if (command == "not")
    {
        writeOutputLine("M=!M");
        writeOutputLine("@SP");
        writeOutputLine("M=M+1");
    }
}

void CodeWriter::writePushPop(Parser::CommandType commandType, std::string segment, int index)
{
    writeOutputLine("// " + std::to_string(commandType) + " " + segment + " " + std::to_string(index));
    if (commandType == Parser::C_POP)
    {
        if (segment == "pointer" || segment == "static")
        {
            writeShortPopCommand(segment, index);
        }
        else
        {
            writeLongPopCommand(segment, index);
        }
    }
    else if (commandType == Parser::C_PUSH)
    {
        writeOutputLine("@" + getSegmentPointer(segment, index));
        if (segment == "constant")
        {
            writeOutputLine("D=A");
        }
        else if (segment == "pointer" || segment == "static")
        {
            writeOutputLine("D=M");
        }
        else
        {
            if (segment == "temp")
            {
                writeOutputLine("D=A");
            }
            else
            {
                writeOutputLine("D=M");
            }
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

void CodeWriter::writeGoto(std::string gotoLabel)
{
    writeOutputLine("// GOTO command");
    writeOutputLine("@" + gotoLabel);
    writeOutputLine("0;JMP");
}

void CodeWriter::close()
{
    outputFile.close();
}

void CodeWriter::writeLabel(std::string label)
{
    outputFile << "(" << label << ")" << std::endl;
}

void CodeWriter::writeOutputLine(std::string command)
{
    outputFile << command << std::endl;
}

void CodeWriter::writeEQGTLTCommand(std::string commandLabel)
{
    writeOutputLine("D=M");
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    writeOutputLine("D=M-D");
    writeOutputLine("M=-1");
    writeOutputLine("@" + commandLabel + std::to_string(labelIndex));
    writeOutputLine("D;J" + commandLabel);
    writeOutputLine("@SP");
    writeOutputLine("A=M");
    writeOutputLine("M=0");
    writeOutputLine(commandLabel + std::to_string(labelIndex));
    labelIndex++;
    writeOutputLine("@SP");
    writeOutputLine("M=M+1");
}

std::string CodeWriter::getSegmentPointer(std::string segmentLabel, int index)
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
    else if (segmentLabel == "static")
    {
        return inputFileName + std::to_string(index);
    }
    else if (segmentLabel == "pointer")
    {
        return index == 0 ? "THIS" : "THAT";
    }
    return std::to_string(index);
}
void CodeWriter::writeShortPopCommand(std::string segmentLabel, int index)
{
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    writeOutputLine("D=M");
    writeOutputLine("@" + getSegmentPointer(segmentLabel, index));
    writeOutputLine("M=D");
}

void CodeWriter::writeLongPopCommand(std::string segmentLabel, int index)
{
    writeOutputLine("@" + getSegmentPointer(segmentLabel, index));
    writeOutputLine(segmentLabel == "temp" ? "D=A" : "D=M");
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

std::string CodeWriter::getFileName(std::string path)
{
    size_t startPos = path.find_last_of("/");
    size_t endPos = path.find_last_of(".") + 1;
    if (startPos == std::string::npos)
    {
        startPos = 0;
    }
    path.erase(endPos, 2);
    return path.substr(startPos);
}