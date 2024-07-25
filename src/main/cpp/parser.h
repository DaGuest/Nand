#ifndef PARSER_H
#define PARSER_H
#include <iostream>;
#include <fstream>;

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
        C_CALL
    };
    Parser();
    Parser(std::string fileName);
    bool hasMoreLines();
    void advance();
    void close();
    CommandType commandType();

private:
    std::ifstream inputFile;
    std::string currentCommand;
};

#endif