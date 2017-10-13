package mcts;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

public class Node {
    State      state;
    Node       parent;
    List<Node> childArray;
    Random     rnd;

    public Node() {
        this.state = new State();
        childArray = new ArrayList<>();
        rnd = new Random();
    }

    public Node(State state) {
        this.state = state;
        childArray = new ArrayList<>();
        rnd = new Random();
    }

    public Node(State state, Node parent, List<Node> childArray) {
        this.state = state;
        this.parent = parent;
        this.childArray = childArray;
        rnd = new Random();
    }

    public Node(Node node) {
        this.state = new State(node.getState());
        if (node.getParent() != null)
            this.parent = node.getParent();
        this.childArray = new ArrayList<>();
        List<Node> childArray = node.getChildArray();
        for (Node child : childArray) {
            this.childArray.add(new Node(child));
        }
        rnd = new Random();
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public Node getParent() {
        return parent;
    }

    public void setParent(Node parent) {
        this.parent = parent;
    }

    public List<Node> getChildArray() {
        return childArray;
    }

    public void setChildArray(List<Node> childArray) {
        this.childArray = childArray;
    }

    public Node getRandomChildNode() {
        int noOfPossibleMoves = this.childArray.size();
        int selectRandom = this.rnd.nextInt(noOfPossibleMoves);
        return this.childArray.get(selectRandom);
    }

    public Node getChildWithMaxScore() {
        return Collections.max(this.childArray, Comparator.comparing(c -> {
            return c.getState().getVisitCount();
        }));
    }
}
