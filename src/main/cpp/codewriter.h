#pragma once
#include <string>
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
    CodeWriter(std::string fileName);

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
     * Closes the outpufile.
     */
    void close();

private:
    std::ofstream outputFile;
    int labelIndex;

    /**
     * Pop the last value of the stack
     * Value is stored in D.
     */
    void writePopCommand();

    /**
     * Pop two two values from the stack
     * First value -> D, second value -> M
     */
    void writeDoublePopCommand();

    /**
     * Write the assembly command: SP+1
     */
    void writeSPStepCommand();

    /**
     * Write a label
     */
    void writeLabel(std::string label);

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
     * @param segmentLabel The string value of the segement label: local, argument, this or that.
     */
    std::string getSegmentPointer(std::string segmentLabel);
};