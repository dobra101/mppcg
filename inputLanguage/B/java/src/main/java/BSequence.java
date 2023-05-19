import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BSequence<T extends Comparable<T>> implements Comparable<BSequence<T>> {
    final List<T> elements;

    public BSequence(List<T> elements) {
        this.elements = elements;
    }

    @SafeVarargs
    public BSequence(T... elements) {
        this.elements = new ArrayList<>();
        this.elements.addAll(Arrays.asList(elements));
    }

    public BSequence<T> append(T element) {
        elements.add(element);
        return this;
    }

    public int card() {
        return elements.size();
    }

    public BSequence<T> restrict_front(int count) {
        List<T> restricted = elements.subList(0, count);
        return new BSequence<>(restricted);
    }

    public BSequence<T> restrict_tail(int count) {
        List<T> restricted = elements.subList(count, elements.size());
        return new BSequence<>(restricted);
    }

    public BSequence<T> concat(BSequence<T> other) {
        List<T> result = new ArrayList<>(elements);
        result.addAll(other.elements);
        return new BSequence<>(result);
    }

    public BSequence<T> union(BRelation<?, T> other) {
        return new BSequence<>(other.entries.stream().map(c -> c.right).toList());
    }

    public BSequence<T> front() {
        return new BSequence<>(elements.subList(0, elements.size()-1));
    }

    public BSequence<T> tail() {
        return new BSequence<>(elements.subList(1, elements.size()));
    }

    public T first() {
        return elements.get(0);
    }

    public T last() {
        return elements.get(elements.size()-1);
    }

    @Override
    public String toString() {
        // TODO: only for execTest
        return elements.toString().replace("[", "{").replace("]", "}");
    }

    @Override
    public int compareTo(final BSequence<T> o) {
        if (elements == o.elements) return 0;
        return -1;
    }
}