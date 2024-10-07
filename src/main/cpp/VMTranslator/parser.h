#ifndef PARSER_H
#define PARSER_H
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

class Parser
{
public:
    enum CommandType
    {
        C_ARITHMETIC,
        C_PUSH,
        C_POP,
        C_LABEL,
        C_GOTO,
        C_IF,
        C_FUNCTION,
        C_RETURN,
        C_CALL,
        C_NONE
    };
    /**
     * Do not use standard constructor. Use Parser(std::string fileName).
     */
    Parser();

    /**
     * A parser that handles .vm files line by line.
     * @param inputPath path to the filename of the xyz.vm file.
     */
    Parser(std::string inputPath);

    /**
     * Checks if there are more lines to be parsed.
     * @return a bool to inidicate if there are more lines to be parsed.
     */
    bool hasMoreLines();

    /**
     * Sets the next line in the file as the current command.
     */
    void advance();

    /**
     * Closes the given .vm file.
     */
    void close();

    /**
     * Returns the commandType of the current command.
     * @return Parser::CommandType that indicates the current command type.
     */
    CommandType commandType();

    /**
     * Returns the first argument of the current command
     * @return The string representation of the first argument.
     * e.g. add, neg, local, foo, ...
     */
    std::string arg1();

    /**
     * Returns the second argument of the current command
     * Should only be called if the current command is C_PUSH, C_POP, C_FUNCTION, C_CALL
     * @return The int representation of the second argument.
     */
    int arg2();

private:
    std::ifstream inputFile;
    std::string currentCommand;
    std::vector<std::string> splitCommands;

    /**
     * Strips and TAB or space characters from the front of a string.
     * @return The stripped string
     */
    std::string stripFront(std::string stringToStrip);

    /**
     * Splits string on whitespace character.
     * The individual string are saved in splitCommands
     */
    void splitString(std::string stringToSplit);

    /**
     * Checks if the current line is a valid line.
     * Invalid lines are comments (//) or blank lines.
     */
    bool checkLine();
};

#endif