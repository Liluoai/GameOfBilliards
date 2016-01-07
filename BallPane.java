package first;

import javafx.scene.layout.Pane;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Rectangle;
import javafx.scene.paint.Color;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;
import javafx.util.Duration;
import java.util.ArrayList;

//这个类是用来构造桌球面板并实现桌球的碰撞的
public class BallPane extends Pane{
	ArrayList<Ball> arrayListBall = new ArrayList<Ball>();
	Timeline timeline;
	
	
	BallPane(){
		Rectangle rectangle1 = new Rectangle(0, 0, 538, 286);
		rectangle1.setArcHeight(20);
		rectangle1.setArcHeight(20);
		rectangle1.setFill(Color.BROWN);		
		Rectangle rectangle2 = new Rectangle(18, 18, 502, 250);
		rectangle2.setFill(Color.DARKGREEN);		
		Circle circle1 = new Circle(18, 18, 12);
		circle1.setFill(Color.BLACK);
		Circle circle2 = new Circle(269, 18, 12);
		circle2.setFill(Color.BLACK);
		Circle circle3 = new Circle(520, 18, 12);
		circle3.setFill(Color.BLACK);
		Circle circle4 = new Circle(18, 268, 12);
		circle4.setFill(Color.BLACK);
		Circle circle5 = new Circle(269, 268, 12);
		circle5.setFill(Color.BLACK);
		Circle circle6 = new Circle(520, 268, 12);
		circle6.setFill(Color.BLACK);
		
		getChildren().addAll(rectangle1, rectangle2, circle1, circle2 ,circle3, circle4, circle5, circle6);
	}
	
	
	void setArrayList(ArrayList<Ball> arrayListBall0){
		for (int i = 0; i < arrayListBall0.size(); i ++){
			arrayListBall.set(i, arrayListBall0.get(i));
		}		
	}
	
	void playTheGameOfMoveAllBall(){
		//刷新频率调高，速度调低，这样就可以有效地避免一个球嵌入另外的球里面了
		timeline = new Timeline(new KeyFrame(Duration.millis(2), e -> moveAllBall()));
		timeline.setCycleCount(Timeline.INDEFINITE);
		timeline.play();
	}
	
	void playTheGameOfPushBall(){
		
	}

	
	
	void moveAllBall(){
		
		//实现球与壁的碰撞弹回，无速度损失。之后最好查阅文献之后模拟出最合适的
		for (int i = 0; i < arrayListBall.size(); i++){
			if (arrayListBall.get(i).getCenterX() <= 18 + arrayListBall.get(i).getRadius() || arrayListBall.get(i).getCenterX() >= 520 - arrayListBall.get(i).getRadius())
				arrayListBall.get(i).setDx(arrayListBall.get(i).getDx() * -1);
			if (arrayListBall.get(i).getCenterY() <= 18 + arrayListBall.get(i).getRadius() || arrayListBall.get(i).getCenterY() >= 268 - arrayListBall.get(i).getRadius())
				arrayListBall.get(i).setDy(arrayListBall.get(i).getDy() * -1);	
		}
		
		
		//新建一个Arraylist类来存放速度的改变。。！！！注意这里就算是这样了，还有最后的两个颜色是引用变量
		ArrayList<Ball> arrayListBall0 = new ArrayList<Ball>();
		for (int jj = 0; jj < arrayListBall.size(); jj ++){
			arrayListBall0.add(new Ball(arrayListBall.get(jj).getCenterX(),arrayListBall.get(jj).getCenterY(),arrayListBall.get(jj).getRadius(),arrayListBall.get(jj).getDx(),arrayListBall.get(jj).getDy(),Color.ALICEBLUE,Color.BLACK));
		}
		for (int i = 0; i < arrayListBall.size(); i++){			
			//对于某个球，每次都与其他所有的球进行位置判定，来改变他的速度实现碰撞。并且每次都将数据先存放在一个临时新建的arraylist类中，等全部判定完成之后再赋值给原来的arraylsit类
			//对于某一个球的多次碰撞判定，必须每次都使用新的速度；对于另一个球的判定，必须使用另外球原来的速度
			//跳过了i球和自身的碰撞判定			
			int j = 0;
			double e = 1; //e是碰撞恢复系数
			for ( ; j < i; j ++){
				if (Math.hypot(arrayListBall.get(i).getCenterX() - arrayListBall.get(j).getCenterX(), arrayListBall.get(i).getCenterY() - arrayListBall.get(j).getCenterY()) <= arrayListBall.get(i).getRadius() + arrayListBall.get(j).getRadius()){
					//碰撞的Dx和Dy要同时改变，所以都得引用本次改变之前的
					double arrayListBall0Dx0 = arrayListBall0.get(i).getDx();
					arrayListBall0.get(i).setDx((arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius()/(arrayListBall0.get(i).getRadius()*arrayListBall0.get(i).getRadius()+arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius())*(1+e)*(arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-(arrayListBall.get(j).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall.get(j).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))))*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) -(-arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))));
					double arrayListBall0Dx1 = arrayListBall0.get(i).getDx();
					arrayListBall0.get(i).setDx(arrayListBall0Dx0);
					arrayListBall0.get(i).setDy((arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius()/(arrayListBall0.get(i).getRadius()*arrayListBall0.get(i).getRadius()+arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius())*(1+e)*(arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-(arrayListBall.get(j).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall.get(j).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))))*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +(-arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))));
					arrayListBall0.get(i).setDx(arrayListBall0Dx1);
				}				
			}
			for (j = i + 1; j < arrayListBall.size(); j++){
				if (Math.hypot(arrayListBall.get(i).getCenterX() - arrayListBall.get(j).getCenterX(), arrayListBall.get(i).getCenterY() - arrayListBall.get(j).getCenterY()) <= arrayListBall.get(i).getRadius() + arrayListBall.get(j).getRadius()){
					double arrayListBall0Dx0 = arrayListBall0.get(i).getDx();
					arrayListBall0.get(i).setDx((arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius()/(arrayListBall0.get(i).getRadius()*arrayListBall0.get(i).getRadius()+arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius())*(1+e)*(arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-(arrayListBall.get(j).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall.get(j).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))))*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) -(-arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))));
					double arrayListBall0Dx1 = arrayListBall0.get(i).getDx();
					arrayListBall0.get(i).setDx(arrayListBall0Dx0);
					arrayListBall0.get(i).setDy((arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius()/(arrayListBall0.get(i).getRadius()*arrayListBall0.get(i).getRadius()+arrayListBall.get(j).getRadius()*arrayListBall.get(j).getRadius())*(1+e)*(arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))-(arrayListBall.get(j).getDx()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())))+arrayListBall.get(j).getDy()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))))*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +(-arrayListBall0.get(i).getDx()*((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))) +arrayListBall0.get(i).getDy()*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))))*((arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())/Math.sqrt((arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY())*(arrayListBall.get(j).getCenterY()-arrayListBall0.get(i).getCenterY()) + (arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX())*(arrayListBall.get(j).getCenterX()-arrayListBall0.get(i).getCenterX()))));
					arrayListBall0.get(i).setDx(arrayListBall0Dx1);				
				}
			}
		}	
		//把改变后的速度还给原来的类
		//bug的原因是之后之后的ball已经不是前面的ball了，所以不会动。总结下！！
		for (int jj = 0; jj < arrayListBall0.size(); jj ++){
			arrayListBall.get(jj).setDx(arrayListBall0.get(jj).getDx());
			arrayListBall.get(jj).setDy(arrayListBall0.get(jj).getDy());			
			arrayListBall.get(jj).setCenterX(arrayListBall0.get(jj).getCenterX() + arrayListBall0.get(jj).getDx());
			arrayListBall.get(jj).setCenterY(arrayListBall0.get(jj).getCenterY() + arrayListBall0.get(jj).getDy());	
		}
		
				
		//实现球的进洞
		for (int i = 0; i < arrayListBall.size(); i ++){
			if (((arrayListBall.get(i).getCenterX()-18)*(arrayListBall.get(i).getCenterX()-18)+(arrayListBall.get(i).getCenterY()-18)*(arrayListBall.get(i).getCenterY()-18) <= 270) || ((arrayListBall.get(i).getCenterX()-269)*(arrayListBall.get(i).getCenterX()-269)+(arrayListBall.get(i).getCenterY()-18)*(arrayListBall.get(i).getCenterY()-18) <= 200) || ((arrayListBall.get(i).getCenterX()-520)*(arrayListBall.get(i).getCenterX()-520)+(arrayListBall.get(i).getCenterY()-18)*(arrayListBall.get(i).getCenterY()-18) <= 270) || ((arrayListBall.get(i).getCenterX()-18)*(arrayListBall.get(i).getCenterX()-18)+(arrayListBall.get(i).getCenterY()-268)*(arrayListBall.get(i).getCenterY()-268) <= 270) || ((arrayListBall.get(i).getCenterX()-269)*(arrayListBall.get(i).getCenterX()-269)+(arrayListBall.get(i).getCenterY()-268)*(arrayListBall.get(i).getCenterY()-268) <= 200) || ((arrayListBall.get(i).getCenterX()-520)*(arrayListBall.get(i).getCenterX()-520)+(arrayListBall.get(i).getCenterY()-268)*(arrayListBall.get(i).getCenterY()-268) <= 270)) {
				if (i !=0){
					arrayListBall.get(i).setOpacity(0);
					arrayListBall.remove(i);
				}
				else {
					arrayListBall.get(i).setCenterX(0);
					arrayListBall.get(i).setCenterY(0);
					arrayListBall.get(i).setDx(0);
					arrayListBall.get(i).setDy(0);
					arrayListBall.get(i).setRadius(1);
				}
			}
		}
		

		//实现球的减速
		for (int i = 0; i < arrayListBall.size(); i ++){
			double decreaseSpeed = 0.0005;
			if (arrayListBall.get(i).getDx() < 2*decreaseSpeed && arrayListBall.get(i).getDx() > -2*decreaseSpeed)
				arrayListBall.get(i).setDx(0);
			else if (arrayListBall.get(i).getDx() > 0)
				arrayListBall.get(i).setDx(arrayListBall.get(i).getDx() - decreaseSpeed*Math.abs(arrayListBall.get(i).getDx())/(Math.abs(arrayListBall.get(i).getDx())+Math.abs(arrayListBall.get(i).getDy())));
			else if (arrayListBall.get(i).getDx() < 0)
				arrayListBall.get(i).setDx(arrayListBall.get(i).getDx() + decreaseSpeed*Math.abs(arrayListBall.get(i).getDx())/(Math.abs(arrayListBall.get(i).getDx())+Math.abs(arrayListBall.get(i).getDy())));
			else ;
			
			if (arrayListBall.get(i).getDy() < 2*decreaseSpeed && arrayListBall.get(i).getDy() > -2*decreaseSpeed)
				arrayListBall.get(i).setDy(0);
			else if (arrayListBall.get(i).getDy() > 0)
				arrayListBall.get(i).setDy(arrayListBall.get(i).getDy() - decreaseSpeed*Math.abs(arrayListBall.get(i).getDy())/(Math.abs(arrayListBall.get(i).getDx())+Math.abs(arrayListBall.get(i).getDy())));
			else if (arrayListBall.get(i).getDy() < 0)
				arrayListBall.get(i).setDy(arrayListBall.get(i).getDy() + decreaseSpeed*Math.abs(arrayListBall.get(i).getDy())/(Math.abs(arrayListBall.get(i).getDx())+Math.abs(arrayListBall.get(i).getDy())));
			else ;
		}

		
		//判定所有球是否都停下来了
		int j = 0;
		for (int i = 0; i < arrayListBall.size(); i ++){
			if (arrayListBall.get(i).getDx() != 0 || arrayListBall.get(i).getDy() != 0)
				j = 1;
		}
		
		//若白球被打进洞了，那么把它放到起点
		if (j==0) {
			for (int i = 0; i < arrayListBall.size(); i ++){
				if (arrayListBall.get(i).getColorOfFill() == Color.WHITE && arrayListBall.get(i).getRadius() == 1){
					arrayListBall.get(i).setCenterX(480);
					arrayListBall.get(i).setCenterY(143);
					arrayListBall.get(i).setDx(0);
					arrayListBall.get(i).setDy(0);
					arrayListBall.get(i).setRadius(10);
				}
					
			}
		}
		//若都停下来了，开始绘制球杆打球
		if (j == 0) {
			int k = 1;
			k *= -1;
			setOpacity(getOpacity() + 0.0001*k);
			timeline.pause();
		}
		
		
	}	
	

}
