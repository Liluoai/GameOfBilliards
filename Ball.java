package first;

import javafx.scene.shape.Circle;
import javafx.scene.paint.Color;

//这个类是用来构造桌球的
public class Ball extends Circle{
	double dx, dy;
	double x, y, r;
	Color colorOfFill;
	Color colorOfStroke;
	
	Ball(){
	}
	Ball(double x, double y, double r, double dx, double dy, Color colorOfFill, Color colorOfStroke){
		super(x, y, r);
		setFill(colorOfFill);
		setStroke(colorOfStroke);
		this.x = x;
		this.y = y;
		this.r = r;
		this.dx = dx;
		this.dy = dy;
		this.colorOfFill = colorOfFill;
		this.colorOfStroke = colorOfStroke;		
	}
	
	//下面不要定义xyr的set和get方法，在Circle中已经给出了，用了会出错，不知道为什么
	void setDx(double dx){
		this.dx = dx;
	}
	void setDy(double dy){
		this.dy = dy;
	}	
	void setColorOfFill(Color colorOfFill){
		this.colorOfFill = colorOfFill;
	}
	void setColorOfStroke(Color colorOfStroke){
		this.colorOfStroke = colorOfStroke;
	}	
	double getDx(){
		return dx;
	}
	double getDy(){
		return dy;
	}
	Color getColorOfFill(){
		return colorOfFill;
	}
	Color getColorOfStroke(){
		return colorOfStroke;
	}
}
