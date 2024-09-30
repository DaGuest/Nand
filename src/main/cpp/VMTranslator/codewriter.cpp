#include "codewriter.h"

CodeWriter::CodeWriter() {};

CodeWriter::CodeWriter(std::string inputPath)
{
    inputPath.erase(inputPath.find_last_of(".") + 1, 2);
    inputFileName = getFileName(inputPath);
    outputFile.open(inputPath + "asm");
    labelIndex = 1;
    returnIndex = 1;
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
            if (index > 0)
            {
                writeOutputLine("@" + std::to_string(index));
                writeOutputLine("A=D+A");
                writeOutputLine("D=M");
            }
        }
        writeFinalPushCommand();
    }
}

void CodeWriter::writeGoto(std::string gotoLabel)
{
    writeOutputLine("// GOTO command");
    writeOutputLine("@" + gotoLabel);
    writeOutputLine("0;JMP");
}

void CodeWriter::writeIf(std::string gotoLabel)
{
    writeOutputLine("// IF-GOTO command");
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    writeOutputLine("D=M");
    writeOutputLine("@" + gotoLabel);
    writeOutputLine("D;JLT");
}

void CodeWriter::writeCall(std::string functionName, int nArgs)
{
    writeOutputLine("// CALL command");
    // Push return address onto stack
    std::string returnLabel = inputFileName + functionName + "$ret." + std::to_string(returnIndex++);
    writeOutputLine("@" + returnLabel);
    writeOutputLine("D=A");
    writeFinalPushCommand();
    // Push lcl, arg, this and that onto stack
    writePushPop(Parser::C_PUSH, "local", 0);
    writePushPop(Parser::C_PUSH, "argument", 0);
    writePushPop(Parser::C_PUSH, "this", 0);
    writePushPop(Parser::C_PUSH, "that", 0);
    // Reposition ARG
    writeOutputLine("// Reposition ARG");
    writeOutputLine("@SP");
    writeOutputLine("D=M");
    writeOutputLine("@5");
    writeOutputLine("D=D-A");
    writeOutputLine("@" + std::to_string(nArgs));
    writeOutputLine("D=D-A");
    writeOutputLine("@ARG");
    writeOutputLine("M=D");
    // Reposition LCL
    writeOutputLine("// Reposition LCL");
    writeOutputLine("@SP");
    writeOutputLine("D=M");
    writeOutputLine("@LCL");
    writeOutputLine("M=D");
    // Goto functionname
    writeGoto(inputFileName + functionName);
    // Inject returnAddress label
    writeLabel(returnLabel);
}

void CodeWriter::writeFunction(std::string functionName, int nVars)
{
    writeOutputLine("// FUNCTION command");
    writeLabel(inputFileName + functionName);
    // push n local vars onto stack with value 0
    for (int i = 0; i < nVars; i++)
    {
        writePushPop(Parser::C_PUSH, "constant", 0);
    }
}

void CodeWriter::writeReturn()
{
    // temp 0 = *(endFrame-5)
    writeReplaceCommand("TEMP", 5);
    // ARG = pop()
    writeOutputLine("@SP");
    writeOutputLine("AMD=M-1");
    writeOutputLine("@ARG");
    writeOutputLine("M=D");
    // SP = ARG+1
    writeOutputLine("D=A+1");
    writeOutputLine("@SP");
    writeOutputLine("M=D");
    // THAT = *(endFrame-1)
    writeReplaceCommand("THAT", 1);
    // THIS = *)endFrame-2)
    writeReplaceCommand("THIS", 2);
    // ARG = *)endFrame-3)
    writeReplaceCommand("ARG", 3);
    // LCL = *)endFrame-4)
    writeReplaceCommand("LCL", 4);
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

void CodeWriter::writeFinalPushCommand()
{
    writeOutputLine("@SP");
    writeOutputLine("A=M");
    writeOutputLine("M=D");
    writeOutputLine("@SP");
    writeOutputLine("M=M+1");
}

void CodeWriter::writeReplaceCommand(std::string label, int steps)
{
    writeOutputLine("@LCL");
    writeOutputLine("D=A");
    writeOutputLine("@" + std::to_string(steps));
    writeOutputLine("A=D-A");
    writeOutputLine("D=M");
    writeOutputLine("@" + label);
    writeOutputLine("M=D");
}

std::string CodeWriter::getFileName(std::string path)
{
    size_t startPos = path.find_last_of("/") + 1;
    size_t endPos = path.find_last_of(".") + 1;
    if (startPos == std::string::npos)
    {
        startPos = 0;
    }
    path.erase(endPos, 2);
    return path.substr(startPos);
}