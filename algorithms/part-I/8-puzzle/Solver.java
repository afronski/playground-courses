public class Solver {
    // Find a solution to the initial board (using the A* algorithm).
    public Solver(Board initial) {
        // TODO: Use MinPQ (algs4.jar).
    }

    // Is the initial board solvable?
    public boolean isSolvable() {

    }

    // Minimum number of moves to solve initial board; -1 if no solution.
    public int moves() {

    }

    // Sequence of boards in a shortest solution; null if no solution.
    public Iterable<Board> solution() {

    }

    public static void main(String[] args) {
        In in = new In(args[0]);

        int N = in.readInt();
        int[][] blocks = new int[N][N];

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                blocks[i][j] = in.readInt();
            }
        }

        Board initial = new Board(blocks);
        Solver solver = new Solver(initial);

        if (!solver.isSolvable()) {
            StdOut.println("No solution possible");
        } else {
            StdOut.println("Minimum number of moves = " + solver.moves());

            for (Board board : solver.solution()) {
                StdOut.println(board);
            }
        }
    }
}