import java.util.ArrayList;
import java.util.List;

public class BRelation {
    private final List<BCouple> entries;

    public BRelation(List<BCouple> entries) {
        this.entries = entries;
    }

    public List<Object> image(BInterval interval) {
        List<Object> result = new ArrayList<>();
        for (BCouple entry : entries) {
            if (entry.left instanceof Integer && interval.contains((Integer) entry.left)) {
                result.add(entry.right);
            }
        }
        return result;
    }
}
