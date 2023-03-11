public class BInterval {
    private final int from;
    private final int to;

    public BInterval(int from, int to) {
        this.from = from;
        this.to = to;
    }

    public boolean contains(int x) {
        return x >= from && x <= to;
    }
}
