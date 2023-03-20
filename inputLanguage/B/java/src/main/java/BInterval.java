import org.jetbrains.annotations.NotNull;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.stream.IntStream;

public class BInterval implements Iterable<Integer> {
    private final int from;
    private final int to;

    public BInterval(int from, int to) {
        this.from = from;
        this.to = to;
    }

    public boolean contains(int x) {
        return x >= from && x <= to;
    }

    @NotNull
    @Override
    public Iterator<Integer> iterator() {
        return IntStream.range(from, to).iterator();
    }

    @Override
    public void forEach(final Consumer<? super Integer> action) {
        IntStream.range(from, to).forEach((IntConsumer) action);
    }

    @Override
    public Spliterator<Integer> spliterator() {
        return IntStream.range(from, to).spliterator();
    }
}
