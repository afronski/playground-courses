public class QuickFindUF {
    public int[] id;
    private int count;

    // Instantiate N isolated components 0 through N-1.
    public QuickFindUF(int N) {
        count = N;
        id = new int[N];
        
        for (int i = 0; i < N; i++) {
            id[i] = i;
        }
    }

    // Return number of connected components.
    public int count() {
        return count;
    }

    // Return component identifier for component containing p.
    public int find(int p) {
        return id[p];
    }

    // Are elements p and q in the same component?
    public boolean connected(int p, int q) {
        return id[p] == id[q];
    }

    // Merge components containing p and q.
    public void union(int p, int q) {
        if (connected(p, q)) return;
        
        int pid = id[p];
        for (int i = 0; i < id.length; i++) {
            if (id[i] == pid) {
                id[i] = id[q]; 
            }
        }
        
        count--;
    }

    public static void main(String[] args) {
        int N = StdIn.readInt();
        QuickFindUF uf = new QuickFindUF(N);

        while (!StdIn.isEmpty()) {
            int p = StdIn.readInt();
            int q = StdIn.readInt();
            
            if (uf.connected(p, q)) {
                continue;
            }
            
            uf.union(p, q);
            
            StdOut.println(uf.count() + " components");        
            
            for (int i = 0; i < N; i++) {
                StdOut.print(uf.id[i] + " ");
            }
            StdOut.println();
        }
        

    }
}