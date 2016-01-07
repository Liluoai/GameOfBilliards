package first;

import javafx.animation.FadeTransition;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.util.Duration;
import java.util.ArrayList;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Ellipse;
import javafx.scene.shape.Line;
import javafx.scene.shape.Polyline;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;


//这个是主程序，用来运行游戏的
public class GameStart extends Application{
	
	double mouseLocationX = 0;
	double mouseLocationY = 0;
	int wetherSecondClicked = 0;
	int mission = 1;
	double pushSpeed  = -1;
	int clickSkillTime = 0;
	int clickPushTime = 0;
	
	double angle=0;
	int animationPlayCount=0;
	int skillIsGoing=0;  //这个等于1表示技能动画正在发动，不允许此时击球
	int animation32Count = 0;//这个来控制技能3动画2的两次触发
	double animation32Distance = 0;// 这个来控制技能3动画2的剑的射出距离


	public void start(Stage primaryStage){
		
		//碰撞面板
		BallPane ballPane = new BallPane();
		ballPane.setStyle("-fx-border-color: gray; -fx-background-color: gray;");
		ballPane.setPrefSize(538, 268);
				
		//用户展示面板
		UserDataPane userDataPane = new UserDataPane();
		userDataPane.setStyle("-fx-border-color: gray; -fx-background-color: gray;");
		userDataPane.setPortrait(new ImageView("图片/盖伦.jpg"));
		userDataPane.setSkill1(new ImageView("图片/致命打击.gif"));
		userDataPane.setSkill2(new ImageView("图片/审判.gif"));
		userDataPane.setSkill3(new ImageView("图片/德玛西亚正义.gif"));
		GridPane.setHalignment(userDataPane.portrait, HPos.CENTER);
		GridPane.setHalignment(userDataPane.skill1, HPos.CENTER);



		
		//进入第一关。记住第一个球一定加的一定是白色的白球！
		ballPane.arrayListBall.addAll(new GameMission(mission));
		ballPane.getChildren().addAll(ballPane.arrayListBall);
		
		//这个动画会在球都停下来的时候暂停，然后使得ballPane的一个绑定属性opacity改变，然后激活下面的监听器
		ballPane.playTheGameOfMoveAllBall();
		
		BorderPane borderPane = new BorderPane();
		borderPane.setStyle("-fx-border-color: gray; -fx-background-color: gray;");
		borderPane.setLeft(ballPane);
		borderPane.setRight(userDataPane);
		Scene scene = new Scene(borderPane);
		
		primaryStage.setScene(scene);
		primaryStage.setTitle("showBallPane");
		primaryStage.show();
		primaryStage.setResizable(false);

		Line lineGun = new Line();
		
		
		//球一旦停下来就执行这个语句
		ballPane.opacityProperty().addListener(e -> {
			System.out.println("游戏进行中...");
			wetherSecondClicked = 0;
			
			//设置可以释放下一个技能和击打下一次球了
			clickSkillTime =0 ;
			clickPushTime = 0;
			
			//对于技能2的效果，归零
			ballPane.arrayListBall.get(0).setRadius(10);
			
			lineGun.setStroke(Color.WHITE);
			lineGun.setStrokeWidth(1);
			ballPane.getChildren().add(lineGun);
			
			//如果此时hp=0，那么游戏结束
			if (userDataPane.hp <= 0){
				for (int i = 0; i<ballPane.arrayListBall.size();i ++){
					ballPane.arrayListBall.get(i).setOpacity(0);
				}	
				Text congradulationText = new Text("游戏结束");
				congradulationText.setX(200);
				congradulationText.setY(100);
				congradulationText.setStrokeWidth(1);
				congradulationText.setFont(Font.font("Times New Roman", FontWeight.BOLD, FontPosture.ITALIC, 40));
				congradulationText.setStroke(Color.BLACK);
				ballPane.getChildren().add(congradulationText);
				FadeTransition congradulation = new FadeTransition(Duration.millis(300));				
				congradulation.setNode(congradulationText);
				congradulation.setFromValue(0);
				congradulation.setToValue(1);		
				congradulation.setCycleCount(Timeline.INDEFINITE);
				congradulation.setAutoReverse(true);
				congradulation.play();				
			}
				
			
			//球停下来的时候，画出球杆和构造辅助线
			ballPane.setOnMouseMoved(e1 -> {
				mouseLocationX = e1.getX();
				mouseLocationY = e1.getY();
				
				if (e1.getX()>18 && e1.getX()<520 && e1.getY()>18 && e1.getY()<268){
					lineGun.setStartX(ballPane.arrayListBall.get(0).getCenterX());
					lineGun.setStartY(ballPane.arrayListBall.get(0).getCenterY());
					lineGun.setEndX(e1.getX());
					lineGun.setEndY(e1.getY());
				}
				//lineGun.setEndX((200/(Math.hypot(mouseLocationX-ballPane.arrayListBall.get(0).getCenterX(), mouseLocationY-ballPane.arrayListBall.get(0).getCenterY()))) * (mouseLocationX - ballPane.arrayListBall.get(0).getCenterX()) + ballPane.arrayListBall.get(0).getCenterX());
				//lineGun.setEndY((200/(Math.hypot(mouseLocationX-ballPane.arrayListBall.get(0).getCenterX(), mouseLocationY-ballPane.arrayListBall.get(0).getCenterY()))) * (mouseLocationY - ballPane.arrayListBall.get(0).getCenterY()) + ballPane.arrayListBall.get(0).getCenterY());
			
			});
			
			//设置击球
			ballPane.setOnMouseClicked(e2 -> {
				//首先判断技能动画是否显示结束了
				if (skillIsGoing==0){
					double sino = (mouseLocationY - ballPane.arrayListBall.get(0).getCenterY()) / (Math.hypot(mouseLocationX - ballPane.arrayListBall.get(0).getCenterX(), mouseLocationY - ballPane.arrayListBall.get(0).getCenterY()));
					double coso = (mouseLocationX - ballPane.arrayListBall.get(0).getCenterX()) / (Math.hypot(mouseLocationX - ballPane.arrayListBall.get(0).getCenterX(), mouseLocationY - ballPane.arrayListBall.get(0).getCenterY()));
					if (wetherSecondClicked == 0){
						ballPane.arrayListBall.get(0).setDx(-pushSpeed * coso);
						ballPane.arrayListBall.get(0).setDy(-pushSpeed * sino);
						ballPane.getChildren().remove(lineGun);
						ballPane.timeline.play();
						wetherSecondClicked = 1;
					}
					pushSpeed = -1;
					
					//同时生命值减去1。
					if (clickPushTime==0){
						userDataPane.hp -= 1;
						userDataPane.setHpText(userDataPane.hp);
						clickPushTime = 1;
					}
					
					//同时将技能和击球设置为不可。直到下次球停下来的时候才能使用技能
					clickPushTime=1;
					clickSkillTime=1;
				}
				
			
			});
			
			
			//每次球一停下来就判定是否过关,要是只剩下白球了就进入下一关
			ballPane.arrayListBall.trimToSize();
			if (ballPane.arrayListBall.size() ==1){
				ballPane.getChildren().remove(ballPane.arrayListBall.get(0));
				ballPane.arrayListBall.clear();
				
				//显示过关动画
				Text congradulationText = new Text("恭喜过关");
				congradulationText.setX(200);
				congradulationText.setY(100);
				congradulationText.setStrokeWidth(1);
				congradulationText.setFont(Font.font("Times New Roman", FontWeight.BOLD, FontPosture.ITALIC, 40));
				congradulationText.setStroke(Color.YELLOW);
				ballPane.getChildren().add(congradulationText);
				FadeTransition congradulation = new FadeTransition(Duration.millis(300));				
				congradulation.setNode(congradulationText);
				congradulation.setFromValue(0);
				congradulation.setToValue(1);		
				congradulation.setCycleCount(6);
				congradulation.setAutoReverse(true);
				congradulation.play();
				
				ballPane.arrayListBall.addAll(new GameMission(++mission));
				ballPane.getChildren().addAll(ballPane.arrayListBall);
				
				//HP和MP回复满
				userDataPane.hp = 10;
				userDataPane.mp = 10;
				userDataPane.missionNumber++;
				userDataPane.setHpText(userDataPane.hp);
				userDataPane.setMpText(userDataPane.mp);
				userDataPane.setMissionNumberText();
			}
		});
		
		//技能说明文字
		userDataPane.skill1.setOnMouseMoved(e11 -> {
			userDataPane.setTuCao("技能说明：野蛮冲撞\n在球停下来的时候，点击该技能，白球会闪烁1.5秒表示技能发动成功。下一次白球的初速度会变成不加技能时的两倍。\n消耗：1点MP");
		});
		userDataPane.skill2.setOnMouseMoved(e12 -> {
			userDataPane.setTuCao("技能说明：巨大化\n在球停下来的时候，点击该技能，球的大小会变成原来的二倍，持续时间一回合。\n消耗：2点MP");
		});
		userDataPane.skill3.setOnMouseMoved(e13 -> {
			userDataPane.setTuCao("技能说明：誓约胜利之剑\n在球停下来的时候，点击该技能，会释放出三把圣剑。之后，所有的红球都会被圣剑的力量震开。\n消耗：3点MP");
		});
		userDataPane.portrait.setOnMouseMoved(e10 -> {
			userDataPane.setTuCao("游戏说明：\n将所有红球打进球洞为游戏目标。\n1.移动鼠标，点击鼠标左键确定，白球会向那个方向打出去；\n2.先点击右下方的三个技能中的一个，再移动鼠标打球，会有特殊效果\n3.每打球一次HP会减少1点，每释放一次技能MP会减少技能对应的消耗，当HP=0时游戏结束，当MP小于消耗时无法发动技能\n祝你游戏愉快！");
		});
		
		//技能1：野蛮冲撞
		userDataPane.skill1.setOnAction(e -> {
			if (userDataPane.mp >0 && clickSkillTime==0){
				skillIsGoing=1;
				pushSpeed *= 2;
				userDataPane.mp -=1;
				clickSkillTime = 1;
				userDataPane.setMpText(userDataPane.mp);
				//设置技能动画：球被添加技能后会闪动1.5秒
				FadeTransition skill1Animation = new FadeTransition(Duration.millis(150));
				skill1Animation.setNode(ballPane.arrayListBall.get(0));
				skill1Animation.setFromValue(1);
				skill1Animation.setToValue(0);
				skill1Animation.setCycleCount(10);
				skill1Animation.setAutoReverse(true);
				skill1Animation.play();			
				skill1Animation.statusProperty().addListener(e2 ->{
					skillIsGoing=0;
				});
			}
			else if (userDataPane.mp <=0 && clickSkillTime==0)
				userDataPane.setTuCao("MP不足，无法发动技能");
		});
		
		//技能2：巨大化
		userDataPane.skill2.setOnAction(e -> {
			if (userDataPane.mp >=2 && clickSkillTime==0){
				skillIsGoing = 1;
				userDataPane.mp -=2;
				clickSkillTime = 1;
				userDataPane.setMpText(userDataPane.mp);
				
				Timeline skill2Animation = new Timeline(new KeyFrame(Duration.millis(50), e22 ->{
					ballPane.arrayListBall.get(0).setRadius(ballPane.arrayListBall.get(0).getRadius()+0.2);
				}));
				skill2Animation.setCycleCount(50);
				skill2Animation.play();
				skill2Animation.statusProperty().addListener(e2->{
					skillIsGoing=0;
				});
			}
			else if (userDataPane.mp <2 && clickSkillTime==0)
				userDataPane.setTuCao("MP不足，无法发动技能");
		});
		
		//技能3：德玛西亚正义

		userDataPane.skill3.setOnAction(e ->{
			if (userDataPane.mp>=3 && clickSkillTime==0){
				skillIsGoing=1;
				userDataPane.mp -=3;
				clickSkillTime = 1;
				userDataPane.setMpText(userDataPane.mp);
				
				//这里是三把剑旋转的动画
				angle=0;
				animationPlayCount=0;
				ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,0,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
				ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,2.3561925,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
				ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,4.712385,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
				Timeline skill3Animation1 = new Timeline(new KeyFrame(Duration.millis(50),e1->{
					angle=angle+0.314;
					ballPane.getChildren().remove(ballPane.getChildren().size()-1);
					ballPane.getChildren().remove(ballPane.getChildren().size()-1);
					ballPane.getChildren().remove(ballPane.getChildren().size()-1);

					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,0+angle,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,2.3561925+angle,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,4.712385+angle,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));

				}));
				skill3Animation1.setCycleCount(20);
				skill3Animation1.play();
				//下面表示旋转动画结束了。接下来要移除剩下的三把剑，载添加三把剑射出的动画，然后添加别的球加速的效果，最后把skillIsGoing置为0
				skill3Animation1.statusProperty().addListener(e3->{
					//移除三把剑
					int[] j = new int[100];
					for (int i = 0; i <ballPane.getChildren().size(); i++){
						if ( ballPane.getChildren().get(i) instanceof Blade )
							j[i]=100;
					}
					for (int i = 1; i < 100; i++){
						if ( j[i]==100 )
						{
							ballPane.getChildren().remove(i);
							ballPane.getChildren().remove(i);
							ballPane.getChildren().remove(i);
							break;
						}
					}
					
					//三把剑射出的动画
					animation32Count=0;
					animation32Distance=0;
					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,0,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,2.3561925,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
					ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,4.712385,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));

					Timeline skill3Animation2 = new Timeline(new KeyFrame(Duration.millis(50), e1 ->{
						
						ballPane.getChildren().remove(ballPane.getChildren().size()-1);
						ballPane.getChildren().remove(ballPane.getChildren().size()-1);
						ballPane.getChildren().remove(ballPane.getChildren().size()-1);
						animation32Count ++;
						//最开始的那一秒左右，先暂停三把剑，让玩家看清楚三把剑
						if (animation32Count<=10){
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,0,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,2.3561925,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20,4.712385,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
						}
						else {
							animation32Distance +=5;
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20+animation32Distance,0,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20+animation32Distance,2.3561925,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
							ballPane.getChildren().add(new Blade(ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY()+20+animation32Distance,4.712385,ballPane.arrayListBall.get(0).getCenterX(),ballPane.arrayListBall.get(0).getCenterY(),Color.BLACK));
						}
						
					}));
					
					skill3Animation2.setCycleCount(40);
					skill3Animation2.play();
					
					//动画结束，移除三把剑
					skill3Animation2.statusProperty().addListener(e32->{
						int[] jj = new int[100];
						for (int i = 0; i <ballPane.getChildren().size(); i++){
							if ( ballPane.getChildren().get(i) instanceof Blade )
								jj[i]=100;
						}
						for (int i = 1; i < 100; i++){
							if ( jj[i]==100 )
							{
								ballPane.getChildren().remove(i);
								ballPane.getChildren().remove(i);
								ballPane.getChildren().remove(i);
								break;
							}
						}
					});
					
					//把skillisgoing置为0
					skill3Animation2.statusProperty().addListener(e4 ->{
						//别的球加速的效果
						wetherSecondClicked=1; //设置此时鼠标不能点击击球
						ballPane.getChildren().remove(lineGun);
						
						for (int i = 1; i < ballPane.arrayListBall.size(); i++){
							double r = Math.hypot(ballPane.arrayListBall.get(0).getCenterX()-ballPane.arrayListBall.get(i).getCenterX(), ballPane.arrayListBall.get(0).getCenterY()-ballPane.arrayListBall.get(i).getCenterY());
							double xDistance = ballPane.arrayListBall.get(i).getCenterX() - ballPane.arrayListBall.get(0).getCenterX();
							double yDistance = ballPane.arrayListBall.get(i).getCenterY() - ballPane.arrayListBall.get(0).getCenterY();
							ballPane.arrayListBall.get(i).setDx(1 * xDistance / r);
							ballPane.arrayListBall.get(i).setDy(1 * yDistance / r);
						}
						ballPane.timeline.play();
						skillIsGoing=0;
					});
					
				});
			}
			
			else if (userDataPane.mp <3 && clickSkillTime==0)
				userDataPane.setTuCao("MP不足，无法发动技能");
		});
		
		
		
	}
	
	public static void main(String[] args) {
		Application.launch(args);
	}

}
