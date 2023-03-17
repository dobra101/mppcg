import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringJoiner;

public class BSequence {
    private final List<Object> elements;

    public BSequence(List<Object> elements) {
        this.elements = elements;
    }

    public BSequence(Object... elements) {
        this.elements = new ArrayList<>();
        this.elements.addAll(Arrays.asList(elements));
    }

    public BSequence append(Object element) {
        elements.add(element);
        return this;
    }

    public int card() {
        return elements.size();
    }

    public BSequence restrict_front(int count) {
        List<Object> restricted = elements.subList(0, count);
        return new BSequence(restricted);
    }

    public BSequence restrict_tail(int count) {
        List<Object> restricted = elements.subList(count, elements.size());
        return new BSequence(restricted);
    }

    @Override
    public String toString() {
        // TODO: only for execTest
        return elements.toString().replace("[", "{").replace("]", "}");
    }
}
