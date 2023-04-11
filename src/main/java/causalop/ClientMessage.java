package causalop;

public class ClientMessage<T> implements Message{
    T message;

    public ClientMessage(T message){
        this.message=message;
    }

    public T getContent(){
        return message;
    }
    @Override
    public int getType() {
        return 1;
    }
}
