import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Parser {
    public enum CommandType {
        A_COMMAND,
        C_COMMAND,
        L_COMMAND
    }

    File file;
    Scanner scanner;
    String currentCommand;


    public Parser(File file) {
        try {
            this.file = file;
            this.scanner = new Scanner(file);
        } catch (FileNotFoundException e) {
        }
    }

    /**
     * Reset the parser to the start of the .asm file.
     */
    public void Reset() {
        try {
            scanner = new Scanner(file);
        } catch (FileNotFoundException e) {
        }
    }

    /**
     * Checks if there is a command on the next line.
     * @return True if there is another command, false otherwise.
     */
    public boolean hasMoreCommands() {
        return this.scanner.hasNextLine();
    }

    /**
     * Checks if there are more commands to parse. 
     * Selects the command on the next line.
     * If the command is a comment of white space, it advance to the next line that contains a valid command. 
     */
    public void advance() {
        if (hasMoreCommands()) {
            currentCommand = scanner.nextLine().strip();
            while (hasMoreCommands() && (currentCommand.length() < 1 || currentCommand.contains("//"))) {
                currentCommand = scanner.nextLine().strip();
            }
        }
    }

    /**
     * Returns the type of the current command.
     * @return A-, C- or L-command.
     */
    public Parser.CommandType commandType() {
        if (currentCommand.startsWith("@")) {
            return Parser.CommandType.A_COMMAND;
        }
        else if (currentCommand.startsWith("(") && currentCommand.endsWith(")")) {
            return Parser.CommandType.L_COMMAND;
        }
        else {
            return Parser.CommandType.C_COMMAND;
        }
    }

    /**
     * Checks the current command's type and returns the address or label.
     * @return String representation of the address or label.
     */
    public String symbol() {
        return switch (commandType()) {
            case A_COMMAND -> currentCommand.substring(1).strip();
            case L_COMMAND -> currentCommand.substring(1, currentCommand.length() - 1).strip();
            default -> null;
        };
    }
    
    /**
     * Finds the dest part of a C-command.
     * @return A string representation of the destination part.
     */
    public String dest() {
        String destValue = "";
        if (commandType() == Parser.CommandType.C_COMMAND) {
            if (currentCommand.contains("=")) {
                int index = currentCommand.indexOf('=');
                destValue = currentCommand.substring(0, index).strip();
            }
        }
        return destValue;
    }

    /**
     * Finds the comp part of a C-command.
     * @return A string representation of the computation part.
     */
    public String comp() {
        String compValue = "";
        if (commandType() == Parser.CommandType.C_COMMAND) {
            if (currentCommand.contains("=")) {
                int beginIndex = currentCommand.indexOf('=') + 1;
                if (currentCommand.contains(";")) {
                    int endIndex = currentCommand.indexOf(';');
                    compValue = currentCommand.substring(beginIndex, endIndex).strip();
                } else {
                    compValue = currentCommand.substring(beginIndex);
                }
            } else {
                if (currentCommand.contains(";")) {
                    int endIndex = currentCommand.indexOf(';');
                    compValue = currentCommand.substring(0, endIndex).strip();
                } else {
                    compValue = currentCommand.substring(0);
                }
            }
        }
        return compValue;
    }

    /**
     * Finds the jump part of a C-command.
     * @return A string representation of the jump part.
     */
    public String jump() {
        String jumpValue = "";
        if (commandType() == Parser.CommandType.C_COMMAND) {
            if (currentCommand.contains(";")) {
                int beginIndex = currentCommand.indexOf(';') + 1;
                jumpValue = currentCommand.substring(beginIndex).strip();
            }
        }
        return jumpValue;
    }
}