import org.jetbrains.annotations.NotNull;

import java.util.*;

public class BSet<T> implements Set<T> {
    private final Set<T> entries;

    public BSet(Set<T> entries) {
        this.entries = entries;
    }

    public int card() {
        return entries.size();
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
}
