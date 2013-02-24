public class Subset {
    public static void main(String[] args) {
        RandomizedQueue<String> queue = new RandomizedQueue<String>();
        int k = Integer.parseInt(args[0]);

        while (!StdIn.isEmpty()) {
            String input = StdIn.readString();
            queue.enqueue(input);
        }

        for (int i = 0; i < k; ++i) {
            StdOut.println(queue.dequeue());
        }
    }
}