package edu.umb.cs.cs410.hw01;
import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;
import org.junit.Test;

public class CalculatorTest{
    @Test
    public void multiply3By4(){
        Calculator cut = new Calculator();
        float expected = 12;
        float actual = cut.multiply(3,4);
        assertThat(actual, is(expected)); }
    @Test
    public void multiply5By6(){
        Calculator cut = new Calculator();
        float expected = 30;
        float actual = cut.multiply(5,6);
        assertThat(actual, is(expected)); }

    @Test
    public void divide3By2(){
        Calculator cut = new Calculator();
        float expected = 1.5f;
        float actual = cut.divide(3,2);
        assertThat(actual, is(expected));
    }

    @Test
    public void divide8By4(){
        Calculator cut = new Calculator();
        float expected = 2f;
        float actual = cut.divide(8,4);
        assertThat(actual, is(expected));
    }


    @Test(expected=IllegalArgumentException.class)
    public void divide5By0(){
        Calculator cut = new Calculator();
        cut.divide(5,0); }
}