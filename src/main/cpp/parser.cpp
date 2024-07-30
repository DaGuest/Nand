#include "parser.h"

Parser::Parser() {};

Parser::Parser(std::string fileName)
{
    inputFile.open(fileName);
}

void Parser::close()
{
    inputFile.close();
}

bool Parser::hasMoreLines()
{
    return inputFile.peek() != EOF;
}

void Parser::advance()
{
    while (hasMoreLines())
    {
        getline(inputFile, currentCommand);
        if (checkLine())
        {
            std::cout << commandType() << std::endl;
        }
        else
        {
            continue;
        }
    }
}

Parser::CommandType Parser::commandType()
{
    std::vector<std::string> splitCommands = splitString(currentCommand);
    // Check for push/pop command
    if (splitCommands.size() > 1)
    {
        if (splitCommands.front() == "push")
        {
            return Parser::C_PUSH;
        }
        else
        {
            return Parser::C_POP;
        }
    }
    // Else: arithmetic command
    else
    {
        return Parser::C_ARITHMETIC;
    }
}

std::string Parser::arg1()
{
    return "";
}

std::string Parser::arg2()
{
    return "";
}

std::vector<std::string> Parser::splitString(std::string stringToSplit)
{
    std::vector<std::string> splitCommands;
    size_t pos = 0;
    std::string command;
    while ((pos = stringToSplit.find(" ")) != std::string::npos)
    {
        command = stringToSplit.substr(0, pos);
        splitCommands.push_back(command);
        stringToSplit.erase(0, pos + 1);
    }
    return splitCommands;
}

bool Parser::checkLine()
{
    return !(currentCommand.front() == '/' || currentCommand.empty());
}