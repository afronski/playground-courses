public class Percolation {
    private int gridSize;
    private WeightedQuickUnionUF uf;
    private boolean[] opened;
    
    private static final int START = 0;
    private int END;
    
    // Create N-by-N grid, with all sites blocked.
    public Percolation(int N) {
        gridSize = N;
        uf = new WeightedQuickUnionUF(gridSize * gridSize + 2);
        opened = new boolean[N * N + 1]; 
        END = N * N + 1;
       
        for(int i = 1; i <= gridSize; ++i) { 
            uf.union(START, xyTo1D(1, i));
            uf.union(END, xyTo1D(gridSize, i));
        }
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
    
    // Open site (row i, column j) if it is not already.
    public void open(int i, int j) {
        validateIndices(i, j);
        
        if (!isOpen(i, j)) {
            int index = xyTo1D(i, j);
            
            opened[index] = true;
            
            if (isOpenSafe(i + 1, j)) { 
                uf.union(index, xyTo1D(i + 1, j));
            }
            if (isOpenSafe(i - 1, j)) { 
                uf.union(index, xyTo1D(i - 1, j));
            }
            if (isOpenSafe(i, j + 1)) { 
                uf.union(index, xyTo1D(i, j + 1));
            }
            if (isOpenSafe(i, j - 1)) { 
                uf.union(index, xyTo1D(i, j - 1));
            }
        }
    }
    
    // Is site (row i, column j) open?
    public boolean isOpen(int i, int j) {
        return opened[xyTo1D(i, j)];
    }
    
    // Is site (row i, column j) full?
    public boolean isFull(int i, int j) {
        return isOpen(i, j) && uf.connected(START, xyTo1D(i, j));
    }
    
    // Does the system percolate?
    public boolean percolates() {
        return uf.connected(START, END);
    }
}