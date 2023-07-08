import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("unchecked")
public class BSet<T> implements Set<T> {
    private final Set<T> entries;

    public BSet(Set<T> entries) {
        this.entries = entries;
    }

    public BSet(BInterval interval) {
        this.entries = new HashSet<>();
        interval.forEach(e -> this.entries.add((T) e));
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

    public BSet<T> subtraction(BSet<T> other) {
        return minus(other);
    }

    // from B2Program
    public <K extends BSet<T>> BSet<K> pow() {
        BSet<K> result = new BSet<>();
        K start = (K) new BSet<T>();
        Queue<K> queue = new LinkedList<>();
        queue.add(start);
        result = result.union(new BSet<K>(start));
        while(!queue.isEmpty()) {
            K currentSet = queue.remove();
            for(T element : this) {
                K nextSet = (K) currentSet.union(new BSet<>(element));
                int previousSize = result.size();
                result = result.union(new BSet<K>(nextSet));
                if(previousSize < result.size()) {
                    queue.add(nextSet);
                }
            }
        }
        return result;
    }

    public <K> BRelation<T, K> mult(BSet<K> other) {
        // TODO: implement
        return null;
    }

    public boolean containsNot(Object other) {
        return !contains(other);
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

    public BSet<T> intersection(BSet<T> other) {
        Set<T> intersection = entries.stream().filter(other::contains).collect(Collectors.toSet());
        return new BSet<>(intersection);
    }

    public T max() {
        return entries.stream().reduce((a, b) -> ((Number) a).longValue() >= ((Number) b).longValue() ? a : b).orElse(null);
    }

    public T min() {
        return entries.stream().reduce((a, b) -> ((Number) a).longValue() <= ((Number) b).longValue() ? a : b).orElse(null);
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
        return entries.toString().replace("[", "{").replace("]", "}");
    }
}


