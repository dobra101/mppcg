import org.jetbrains.annotations.NotNull;

import java.util.*;

public class BRelation implements Set<BCouple> {
    private final List<BCouple> entries;

    public BRelation(List<BCouple> entries) {
        this.entries = entries;
    }

    public BRelation(BCouple... entries) {
        this.entries = new ArrayList<>();
        this.entries.addAll(Arrays.asList(entries));
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

    public int card() {
        return entries.size();
    }

    public BRelation forwardComposition(BRelation other) {
        List<BCouple> composition = new ArrayList<>();
        for (BCouple entry : entries) {
            List<BCouple> list = other.entries.stream()
                    .filter(c -> c.left == entry.right)
                    .map(c -> new BCouple(entry.left, c.right))
                    .toList();
            composition.addAll(list);
        }
        return new BRelation(composition);
    }

    public Object get(Object key) {
        for (BCouple entry : entries) {
            if (entry.left == key) return entry.right;
        }
        return null;
    }

    @Override
    public int size() {
        return entries.size();
    }

    @Override
    public boolean isEmpty() {
        return entries.isEmpty();
    }

    @Override
    public boolean contains(final Object o) {
        return entries.contains(o);
    }

    @NotNull
    @Override
    public Iterator<BCouple> iterator() {
        return entries.iterator();
    }

    @NotNull
    @Override
    public Object[] toArray() {
        return entries.toArray();
    }

    @NotNull
    @Override
    public <T> T[] toArray(@NotNull final T[] a) {
        return entries.toArray(a);
    }

    @Override
    public boolean add(final BCouple bCouple) {
        return entries.add(bCouple);
    }

    @Override
    public boolean remove(final Object o) {
        return entries.remove(o);
    }

    @Override
    public boolean containsAll(@NotNull final Collection<?> c) {
        return entries.containsAll(c);
    }

    @Override
    public boolean addAll(@NotNull final Collection<? extends BCouple> c) {
        return entries.addAll(c);
    }

    @Override
    public boolean retainAll(@NotNull final Collection<?> c) {
        return entries.retainAll(c);
    }

    @Override
    public boolean removeAll(@NotNull final Collection<?> c) {
        return entries.removeAll(c);
    }

    @Override
    public void clear() {
        entries.clear();
    }

    @Override
    public boolean equals(final Object o) {
        return entries.equals(o);
    }

    @Override
    public int hashCode() {
        return entries.hashCode();
    }
}
