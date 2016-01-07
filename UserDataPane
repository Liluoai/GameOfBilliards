package first;

import javafx.geometry.HPos;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.text.Text;


//这个是用来构造右方的展示面板的
public class UserDataPane extends GridPane{
	Button portrait = new Button();
	Button skill1 = new Button();
	Button skill2 = new Button();
	Button skill3 = new Button();
	int hp=10;
	int mp=10;
	int missionNumber=1;
	Text hpText = new Text("HP: ❤❤❤❤❤❤❤❤❤❤");
	Text mpText = new Text("MP: 卍卍卍卍卍卍卍卍卍卍");
	Text missionNumberText = new Text("关卡："+missionNumber);
	TextArea tuCao = new TextArea();
	
	//注意这里没有空的构造方法
	UserDataPane(){
		super();
		add(portrait,0,0);
		GridPane smallPane = new GridPane();
		smallPane.add(skill1,0,0);
		smallPane.add(skill2,1,0);
		smallPane.add(skill3,2,0);
		add(tuCao,0,1);
		tuCao.setPrefSize(55, 70);
		tuCao.setWrapText(true);
		tuCao.setEditable(false);
		add(smallPane,0,2);
		add(hpText,0,3);
		add(mpText,0,4);
		add(missionNumberText,0,5);
		setHgap(2);
		setVgap(2);
		
	}
	
	void setPortrait(ImageView image){
		portrait.setGraphic(image);
	}
	
	void setSkill1(ImageView image){
		skill1.setGraphic(image);
	}
	void setSkill2(ImageView image){
		skill2.setGraphic(image);
	}
	void setSkill3(ImageView image){
		skill3.setGraphic(image);
	}
	void setHp(int hp){
		this.hp = hp;
	}
	int getHp(){
		return hp;
	}
	void setMp(int mp){
		this.mp = mp;
	}
	int getMp(){
		return mp;
	}
	
	void setHpText(int n){
		StringBuilder a = new StringBuilder("HP:");
		for (int i=1; i<=n;i++){
			a.append("❤");
		}
		hpText.setText(a.toString());
	}
	void setMpText(int n){
		StringBuilder a = new StringBuilder("MP:");
		for (int i=1; i<=n;i++){
			a.append("卍");
		}
		mpText.setText(a.toString());
	}
	
	void setMissionNumber(int n){
		missionNumber =n;
	}
	int getMissionNumber(){
		return missionNumber;
	}
	void setMissionNumberText(){
		missionNumberText.setText("关卡："+missionNumber);
	}
	void setTuCao(String s){
		tuCao.setText(s);
	}
	
	
}


















