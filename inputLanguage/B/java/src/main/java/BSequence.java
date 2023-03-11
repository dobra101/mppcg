import java.util.Arrays;
import java.util.List;

public class BSequence {
    private List<Object> elements;

    public BSequence(Object... elements) {
        this.elements = Arrays.stream(elements).toList();
    }

    public BSequence append(Object element) {
        elements.add(element);
        return this;
    }

    public int card() {
        return elements.size();
    }
}
