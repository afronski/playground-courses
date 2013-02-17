public class Percolation {
    private static final int START = 0;    
    
    private int gridSize;
    private WeightedQuickUnionUF uf;
    private WeightedQuickUnionUF ufFull;
    
    private boolean[] opened;
    private int END;
    
    // Create N-by-N grid, with all sites blocked.
    public Percolation(int N) {
        gridSize = N;
        uf = new WeightedQuickUnionUF(gridSize * gridSize + 2);
        ufFull = new WeightedQuickUnionUF(gridSize * gridSize + 1);
        
        opened = new boolean[N * N + 1]; 
        END = N * N + 1;
    }
    
    // Private helpers.
    private void validateIndices(int x, int y) {
        if (x < 1 || x > gridSize) {
            throw new java.lang.IndexOutOfBoundsException("X index out of bounds");
        }
        
        if (y < 1 || y > gridSize) {
            throw new java.lang.IndexOutOfBoundsException("Y index out of bounds");
        }
    }
    
    private int xyTo1D(int x, int y) {
        return (y - 1) * gridSize + x;
    }
    
    private boolean validateIndiciesSafe(int x, int y) {
        if (x < 1 || x > gridSize) {
            return false;
        }
        
        if (y < 1 || y > gridSize) {
            return false;
        }
        
        return true;
    }    
    
    private boolean isOpenSafe(int i, int j) {
        if (!validateIndiciesSafe(i, j)) {
            return false;
        }
        
        return opened[xyTo1D(i, j)];
    }
    
    private void handleVirtualNodes(int i, int j) {
        // Virtual top.
        if (i == 1) {
            uf.union(START, xyTo1D(i, j));
            ufFull.union(START, xyTo1D(i, j));
        }
        
        // Virtual bottom.
        if (i == gridSize) {
            uf.union(END, xyTo1D(i, j));
        }
    }
    
    private void union(int from, int to) {
        uf.union(from, to);
        ufFull.union(from, to);
    }
    
    // Open site (row i, column j) if it is not already.
    public void open(int i, int j) {
        validateIndices(i, j);
        
        if (!isOpen(i, j)) {
            int index = xyTo1D(i, j);
            
            opened[index] = true;
            handleVirtualNodes(i, j);            
            
            if (isOpenSafe(i + 1, j)) { 
                union(index, xyTo1D(i + 1, j));
            }
            
            if (isOpenSafe(i - 1, j)) { 
                union(index, xyTo1D(i - 1, j));
            }
            
            if (isOpenSafe(i, j + 1)) { 
                union(index, xyTo1D(i, j + 1));
            }
            
            if (isOpenSafe(i, j - 1)) { 
                union(index, xyTo1D(i, j - 1));
            }
        }
    }
    
    // Is site (row i, column j) open?
    public boolean isOpen(int i, int j) {
        validateIndices(i, j);
        return opened[xyTo1D(i, j)];
    }
    
    // Is site (row i, column j) full?
    public boolean isFull(int i, int j) {
        validateIndices(i, j);
        return isOpen(i, j) && ufFull.connected(START, xyTo1D(i, j));
    }
    
    // Does the system percolate?
    public boolean percolates() {
        return uf.connected(START, END);
    }
}