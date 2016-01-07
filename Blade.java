package first;
import javafx.collections.ObservableList;
import javafx.scene.paint.Color;
import javafx.scene.shape.Polygon;


//这个类是用来构造技能3的圣剑的图像和旋转效果的
public class Blade extends Polygon{
	
	Blade(double x, double y, double angle, double originX, double originY, Color color){
		super();
		ObservableList<Double> list = super.getPoints();
		list.add(x-5);
		list.add(y);
		list.add(x-5);
		list.add(y+35);
		list.add(x-35);
		list.add(y+35);
		list.add(x-45);
		list.add(y+55);
		list.add(x-35);
		list.add(y+45);
		list.add(x-10);
		list.add(y+45);
		list.add(x-10);
		list.add(y+145);
		list.add(x);
		list.add(y+165);
		list.add(x+10);
		list.add(y+145);
		list.add(x+10);
		list.add(y+45);
		list.add(x+35);
		list.add(y+45);
		list.add(x+45);
		list.add(y+55);
		list.add(x+35);
		list.add(y+35);
		list.add(x+5);
		list.add(y+35);
		list.add(x+5);
		list.add(y);	
		
		newList(list, angle, originX, originY);
		
		super.setFill(color);
		super.setOpacity(0.5);
	}
	
	
	//注意两个点不能重合。x和y表示旋转前的点，x0y0表示旋转的中点，注意fai是弧度制
	static double newX(double fai, double x, double y, double x0, double y0){
		double r = Math.hypot(x-x0,y-y0);
		double sin0 = (y - y0) / r;
		double cos0 = (x - x0) / r;
		double cos1 = cos0 * Math.cos(fai) - sin0 * Math.sin(fai);
		double x1 = x0 + r * cos1;
		return x1;
	}
	
	static double newY(double fai, double x, double y, double x0, double y0){
		double r = Math.hypot(x-x0,y-y0);
		double sin0 = (y - y0) / r;
		double cos0 = (x - x0) / r;
		double sin1 = sin0 * Math.cos(fai) + cos0 * Math.sin(fai);
		double y1 = y0 + r * sin1;
		return y1;
	}
	
	//这里注意不要把改变后的x用来改变同一个点的y.这是顺时针旋转
	static ObservableList<Double> newList(ObservableList<Double> oldList, double fai, double x0, double y0){
		double[] oldX = new double[oldList.size()];
		for (int i = 0;i < oldList.size();i=i+2){
			oldX[i] = oldList.get(i);
			oldList.set(i, newX(fai, oldList.get(i), oldList.get(i+1), x0, y0));
		}
		for (int i = 1;i < oldList.size();i=i+2){
			oldList.set(i, newY(fai, oldX[i-1], oldList.get(i), x0, y0));
		}
		
		return oldList;
	}
	
	
	
	

}
