public class Solver {
    private SearchNode result;

    private class SearchNode implements Comparable<SearchNode> {
        private final Board board;
        private final int moves;
        private final SearchNode previous;
        private final int priority;

        private SearchNode(Board b, SearchNode p) {
            board = b;
            previous = p;

            if (previous == null) {
                moves = 0;
            } else {
                moves = previous.moves + 1;
            }

            priority = board.manhattan() + moves;
        }

        @Override
        public int compareTo(SearchNode that) {
            return this.priority - that.priority;
        }
    }

    // Find a solution to the initial board (using the A* algorithm).
    public Solver(Board initial) {
        if (initial.isGoal()) {
            result = new SearchNode(initial, null);
        } else {
            result = solve(initial, initial.twin());
        }
    }

    private SearchNode step(MinPQ<SearchNode> queue) {
        SearchNode least = queue.delMin();

        for (Board neighbor: least.board.neighbors()) {
            if (least.previous == null || !neighbor.equals(least.previous.board)) {
                queue.insert(new SearchNode(neighbor, least));
            }
        }

        return least;
    }

    private SearchNode solve(Board initial, Board twin) {
        SearchNode last;

        MinPQ<SearchNode> queue = new MinPQ<SearchNode>();
        MinPQ<SearchNode> twins = new MinPQ<SearchNode>();

        queue.insert(new SearchNode(initial, null));
        twins.insert(new SearchNode(twin, null));

        while (true) {
            last = step(queue);

            if (last.board.isGoal()) {
                return last;
            }

            if (step(twins).board.isGoal()) {
                return null;
            }
        }
    }

    // Is the initial board solvable?
    public boolean isSolvable() {
        return result != null;
    }

    // Minimum number of moves to solve initial board; -1 if no solution.
    public int moves() {
        if (result != null) {
            return result.moves;
        }

        return -1;
    }

    // Sequence of boards in a shortest solution; null if no solution.
    public Iterable<Board> solution() {
        if (result == null) {
            return null;
        }

        Stack<Board> stack = new Stack<Board>();

        for (SearchNode node = result; node != null; node = node.previous) {
            stack.push(node.board);
        }

        return stack;
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