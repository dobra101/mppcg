import org.jetbrains.annotations.NotNull;

import java.util.*;

@SuppressWarnings("unchecked")
public class BSet<T> implements Set<T> {
    private final Set<T> entries;

    public BSet(Set<T> entries) {
        this.entries = entries;
    }

    @SuppressWarnings("unchecked")
    public BSet(T... entries) {
        this.entries = new HashSet<>();
        this.entries.addAll(Arrays.asList(entries));
    }

    public BSet<T> union(BSet<T> other) {
        Set<T> result = new HashSet<>(entries);
        result.addAll(other.entries);
        return new BSet<>(result);
    }

    public int card() {
        return entries.size();
    }

    public BSet<T> plus(BSet<T> other) {
        Set<T> result = new HashSet<>(entries);
        result.addAll(other.entries);
        return new BSet<>(result);
    }

    public BSet<T> minus(BSet<T> other) {
        Set<T> result = new HashSet<>(entries);
        result.removeAll(other.entries);
        return new BSet<>(result);
    }

    public BSequence concat() {
        List<Object> result = new ArrayList<>();
        List<BCouple<?, ?>> couples = new ArrayList<>((List<BCouple<?, ?>>) entries.stream().peek(entry -> {
            if (!(entry instanceof BCouple<?, ?>)) {
                throw new RuntimeException("Cannot concat elements of type " + entry.getClass());
            }
        }).toList());
        couples.sort(new BCoupleComparator());

        for (BCouple<?, ?> couple : couples) {
            result.addAll((Collection<?>) couple.right);
        }
        return new BSequence(result);
    }

    private static class BCoupleComparator implements Comparator<BCouple<?, ?>> {
        @Override
        public int compare(final BCouple o1, final BCouple o2) {
            return ((Integer) o1.left).compareTo((Integer) o2.left);
        }
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
    public Iterator<T> iterator() {
        return entries.iterator();
    }

    @NotNull
    @Override
    public Object[] toArray() {
        return entries.toArray();
    }

    @NotNull
    @Override
    public <E> E[] toArray(@NotNull final E[] a) {
        return entries.toArray(a);
    }

    @Override
    public boolean add(final T o) {
        return entries.add(o);
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
    public boolean addAll(@NotNull final Collection<? extends T> c) {
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
        if (!(o instanceof BSet<?>)) {
            return false;
        }
        return entries.equals(o);
    }

    @Override
    public int hashCode() {
        return entries.hashCode();
    }

    @Override
    public String toString() {
        // TODO: only for execTest
        return entries.toString().replace("[", "{").replace("]", "}");
    }
}


