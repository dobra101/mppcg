import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class BTypes {
    public static Number min(Collection<Number> collection) {
        Number min = Long.MAX_VALUE;
        for (Number entry: collection) {
            if (entry instanceof Integer && (Integer) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Double && (Double) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Long && (Long) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Float && (Float) entry < min.longValue()) {
                min = entry;
            }
        }
        return min;
    }

    public static class BInterval {
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

    public static class BRelation {
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

    public static class BCouple {
        private final Object left;
        private final Object right;

        public BCouple(Object left, Object right) {
            this.left = left;
            this.right = right;
        }
    }
}