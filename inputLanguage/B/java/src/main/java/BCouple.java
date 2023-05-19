import java.util.*;

@SuppressWarnings("unchecked")
public class BCouple<K, V extends Comparable<V>> {
    final K left;
    final V right;

    public BCouple(K left, V right) {
        this.left = left;
        this.right = right;
    }

    @SafeVarargs
    public final V concat(BCouple<K, V>... couples) {
        List<V> result = new ArrayList<>();
        List<BCouple<K, V>> coupleList = new ArrayList<>(List.of(couples));
        coupleList.add(this);
        coupleList.sort(new BCoupleComparator());

        for (BCouple<K, V> couple : coupleList) {
            result.addAll((Collection<? extends V>) ((BSequence<?>) couple.right).elements);
        }

        return (V) new BSequence<>(result);
    }

    public K prj1() {
        return left;
    }

    public V prj2() {
        return right;
    }

    // TODO: only for test cases
    @Override
    public String toString() {
        return "(" + left + " |-> " + right + ")";
    }

    private static class BCoupleComparator implements Comparator<BCouple<?, ?>> {
        @Override
        public int compare(final BCouple<?, ?> o1, final BCouple<?, ?> o2) {
            return ((Integer) o1.left).compareTo((Integer) o2.left);
        }
    }
}