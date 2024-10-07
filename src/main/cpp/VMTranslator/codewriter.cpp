#include "codewriter.h"

CodeWriter::CodeWriter() {};

CodeWriter::CodeWriter(std::string inputPath)
{
    path = inputPath;
    outputFile.open(path + ".asm");
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

void CodeWriter::writePushPop(Parser::CommandType commandType, std::string segment, int index, bool addrOnly)
{
    std::string commentTitle = commandType == Parser::C_PUSH ? "PUSH" : "POP";
    writeOutputLine("// " + commentTitle + " " + segment + " " + std::to_string(index));
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
                writeOutputLine("@" + std::to_string(index));
                writeOutputLine("A=D+A");
                addrOnly ? writeOutputLine("D=A") : writeOutputLine("D=M");
            }
        }
        writeFinalPushCommand();
    }
}

void CodeWriter::writeGoto(std::string gotoLabel)
{
    writeOutputLine("// GOTO command");
    writeOutputLine("@" + inputFileName + "." + scopeName + "$" + gotoLabel);
    writeOutputLine("0;JMP");
}

void CodeWriter::writeIf(std::string gotoLabel)
{
    writeOutputLine("// IF-GOTO command");
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    writeOutputLine("D=M");
    writeOutputLine("@" + inputFileName + "." + scopeName + "$" + gotoLabel);
    writeOutputLine("D;JNE");
}

void CodeWriter::writeCall(std::string functionName, int nArgs)
{
    writeOutputLine("// CALL command");
    // Push return address onto stack
    std::string returnLabel = inputFileName + "." + scopeName + "$ret." + std::to_string(returnIndex++);
    writeOutputLine("@" + returnLabel);
    writeOutputLine("D=A");
    writeFinalPushCommand();
    // Push lcl, arg, this and that onto stack
    writePushPop(Parser::C_PUSH, "local", 0, true);
    writePushPop(Parser::C_PUSH, "argument", 0, true);
    writePushPop(Parser::C_PUSH, "this", 0, true);
    writePushPop(Parser::C_PUSH, "that", 0, true);
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
    writeOutputLine("@" + functionName);
    writeOutputLine("0;JMP");
    // Inject returnAddress label
    writeOutputLine("(" + returnLabel + ")");
}

void CodeWriter::writeFunction(std::string functionName, int nVars)
{
    scopeName = functionName;
    writeOutputLine("// FUNCTION command");
    writeOutputLine("(" + inputFileName + "." + scopeName + ")");
    // push n local vars onto stack with value 0
    for (int i = 0; i < nVars; i++)
    {
        writePushPop(Parser::C_PUSH, "constant", 0);
    }
}

void CodeWriter::writeReturn()
{
    writeOutputLine("// RETURN command");
    // Save return address
    writeReplaceCommand("TEMP", 5);
    // Pop return value from stack and save in ARG
    writeOutputLine("@SP");
    writeOutputLine("AM=M-1");
    writeOutputLine("D=M");
    writeOutputLine("@ARG");
    writeOutputLine("A=M");
    writeOutputLine("M=D");
    // Set SP to ARG + 1
    writeOutputLine("D=A+1");
    writeOutputLine("@SP");
    writeOutputLine("M=D");
    // Restore THAT
    writeReplaceCommand("THAT", 1);
    // Restore THIS
    writeReplaceCommand("THIS", 2);
    // Restore ARG
    writeReplaceCommand("ARG", 3);
    // Restore LCL
    writeReplaceCommand("LCL", 4);
    // Goto returnAddress
    writeOutputLine("@TEMP");
    writeOutputLine("A=M");
    writeOutputLine("0;JMP");
}

void CodeWriter::writeBootStrap()
{
    writeOutputLine("// BOOTSTRAP");
    // Set SP to 256
    writeOutputLine("@256");
    writeOutputLine("D=A");
    writeOutputLine("@0");
    writeOutputLine("M=D");
    // Call Sys.init
    writeCall(inputFileName + ".init", 0);
}

void CodeWriter::setFileName(std::string fileName)
{
    labelIndex = 1;
    returnIndex = 1;
    inputFileName = fileName;
}

void CodeWriter::close()
{
    outputFile.close();
}

void CodeWriter::writeLabel(std::string label)
{
    writeOutputLine("(" + inputFileName + "." + scopeName + "$" + label + ")");
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
    writeOutputLine("(" + commandLabel + std::to_string(labelIndex) + ")");
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
        return inputFileName + "." + std::to_string(index);
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
    writeOutputLine("D=M");
    writeOutputLine("@" + std::to_string(steps));
    writeOutputLine("A=D-A");
    writeOutputLine("D=M");
    writeOutputLine("@" + label);
    writeOutputLine("M=D");
}