public final class BBoolean {
    private final boolean value;

    public BBoolean(boolean value) {
        this.value = value;
    }

    public BBoolean() {
        this.value = true;
    }

    public BBoolean implies(boolean other) {
        return new BBoolean(!value || other);
    }

    public BBoolean implies(BBoolean other) {
        return implies(other.value);
    }

    public boolean value() {
        return value;
    }
}