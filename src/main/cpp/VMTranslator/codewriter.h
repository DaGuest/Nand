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
     * Closes the outpufile.
     */
    void close();

private:
    std::string inputFileName;
    std::ofstream outputFile;
    int labelIndex;

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
     * A helper function that retrieves the stem filename from the given path.
     * @param path The file path in string format.
     * @return The filename stem with a '.' at the end.
     */
    std::string getFileName(std::string path);
};