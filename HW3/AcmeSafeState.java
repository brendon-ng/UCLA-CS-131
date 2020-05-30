import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
	int sz = value.length();
	long[] curr = new long[sz];
	for(int i=0; i<sz; i++){
	    curr[i] = value.get(i);
	}
	return curr;
    }

    public void swap(int i, int j) {
	value.decrementAndGet(i);
	value.incrementAndGet(j);
    }
}
