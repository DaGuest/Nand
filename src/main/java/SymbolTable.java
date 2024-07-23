import java.util.HashMap;

public class SymbolTable {
    HashMap<String, Integer> table;

    public SymbolTable() {
        this.table = new HashMap<>();
    }
    /**
     * Add and entry into the symbol table.
     * @param symbol The symbol representation.
     * @param address The address value of the symbol.
     */
    public void addEntry(String symbol, int address) {
        table.put(symbol, address);
    }

    /**
     * Checks if the symbol exists in the symbol table.
     * @param symbol The symbol representation.
     * @return True if the symbol is in the table, false otherwise.
     */
    public boolean contains(String symbol) {
        return table.containsKey(symbol);
    }

    /**
     * Gives the address of the given symbol.
     * @param symbol The symbol representation.
     * @return The address value of the given symbol, null if it is not in the table.
     */
    public int getAddress(String symbol) {
        return table.get(symbol);
    }
}
