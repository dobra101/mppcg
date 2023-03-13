import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BSequence {
    private List<Object> elements;

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
}
