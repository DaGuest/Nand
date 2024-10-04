#pragma once
#include <string>
#include <filesystem>
#include <algorithm>
#include "parser.h"

class CodeWriter
{
public:
    CodeWriter();
    /**
     * Generates assembly code from the parsed VM command.
     * Outputs assembly code to the given output file.
     * @param fileName path to the output file.
     */
    CodeWriter(std::string inputPath);

    /**
     * Writes to the output file the assembly code that implements the given arithmetic-logical command.
     * @param command The specification of the arithmetic command, e.g. add, sub, eq...
     */
    void writeArithmetic(std::string command);

    /**
     * Write to the output file the assembly code that implements the given push or pop command.
     * @param command Either C_PUSH or C_POP
     * @param segment String representation of the segment where to Push of Pop from, e.g. local, constant
     * @param index Specifies the index to pop from or push to.
     */
    void writePushPop(Parser::CommandType commandType, std::string segment, int index);

    /**
     * Writes to the output file the assembly code that implements the given goto command.
     */
    void writeGoto(std::string gotoLabel);

    /**
     * Writes the assembly code for the if-goto command.
     */
    void writeIf(std::string gotoLabel);

    /**
     * Writes the assembly code for the call command.
     */
    void writeCall(std::string functionName, int nArgs);

    /**
     * Writes the assembly code for the FUNCTION command.
     */
    void writeFunction(std::string funtionName, int nVars);

    /**
     * Writes the assembly code for the RETURN command.
     */
    void writeReturn();

    /**
     * Sets the current fileName of the .vm file that is being translated.
     * @param fileName The stem of the fileName to be used (without extensions or path chars)
     */
    void setFileName(std::string fileName);

    /**
     * Closes the outpufile.
     */
    void close();

    /**
     * Write a label
     */
    void writeLabel(std::string label);

private:
    std::string path;
    std::string inputFileName;
    std::ofstream outputFile;
    int labelIndex;
    int returnIndex;

    /**
     * Write a line into the output file.
     */
    void writeOutputLine(std::string command);

    /**
     * Write EQ, GT or LT command
     * @param command The label of either EQ, GT or LT
     */
    void writeEQGTLTCommand(std::string commandLabel);

    /**
     * Return the standard address of a given segment
     * @param segmentLabel The string value of the segment label: local, argument, this or that.
     */
    std::string getSegmentPointer(std::string segmentLabel, int index);

    /**
     * Write a pop command for POINTER and STATIC commands
     * @param segmentLabel The string value of the segment label.
     */
    void writeShortPopCommand(std::string segmentLabel, int index);

    /**
     * Write a pop command for LCL, ARG, THIS and THAT commands.
     * @param segmentLabel The string value of the segment label.
     */
    void writeLongPopCommand(std::string segmentLabel, int index);

    /**
     * Writes the value in D onto the stack
     */
    void writeFinalPushCommand();

    /**
     * Write a LABEL = *(endFrame - x) command where x is the amount of steps back from endFrame.
     * @param label The label that will have it's address replaced.
     * @param steps The numbers of steps to take back from endFrame.
     */
    void writeReplaceCommand(std::string label, int steps);
};