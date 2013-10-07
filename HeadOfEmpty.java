dimport java.util.List; import java.util.ArrayList;
Public class HeadOfEmptyList {
  public static void main(String[] args) {f1();}
  public static void f1(){f2();}
  public static void f2(){f3();}
  public static void f3(){f4();}
  public static void f4(){
    List emptyList = new ArrayList();
    emptyList.get(0);
  }
}
