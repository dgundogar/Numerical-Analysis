#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAX 50

float Function(float dizi[], int derece, float sayi){
	float carpim, toplam;
	toplam = 0;
	int i, j;
	// burada aralýklarý fonksiyona gönderip sonuç alýyorum
	for(i = 0; i <= derece ; i++){
		carpim = 1;
		for(j = 0; j < i ; j++){
			carpim = carpim * sayi;
		}
		toplam = toplam + carpim * dizi[i];
	}
	return toplam;
}


//türev 
float FunctionDerivative(float dizi[], int derece, float sayi){
	float carpim, toplam;
	toplam = 0;
	int i, j;

	for(i = 0; i <= derece ; i++){
		carpim = 1;
		for(j = 0; j < i-1 ; j++){
			carpim = carpim * sayi;
		}
		toplam = toplam + carpim * dizi[i] * i;
	}
	return toplam;

}
// faktöriyel 

 int factorial(int n) {
    if (n>=1)
        return n*factorial(n-1);
    else
        return 1;
}

float derivate(float dizi[],int  derece, float nokta, float h, int karar){
	float result;
	
	if(karar == 1){
		result = (Function(dizi, derece, (nokta)) - Function(dizi, derece, (nokta - h))) / (h);
	}
	else if(karar == 2){
		result = (Function(dizi, derece, (nokta + h)) - Function(dizi, derece, (nokta))) / (h);
	}
	else{
		result = (Function(dizi, derece, (nokta + h)) - Function(dizi, derece, (nokta - h))) / (2*h);
	}
	
	return result;
	
}


// gauss eliminasyon

void GaussElimination(){
	int i,j,n,a,m,h,b;
	float dizi[MAX],result[MAX];
  	float order,carpim,matrix[MAX][MAX];
  	
  	printf("Denklem sayisini giriniz: \n");
	scanf("%d", &n);
	
	for(i=0; i<n; i++){
		for(j=0;j<n;j++){
			printf("%d. denklemin  %d. katsayisi: \n", i+1, j);
				scanf("%f", &matrix[i][j]);
		}
	}
	printf("Denklem sonuclarini giriniz: \n");
  	for(i=0;i<n;i++){
    	printf("%d. denklem: \n", i+1);
    	scanf("%f", &dizi[i]);
  	}
	
	for(j=0;j<n;j++){
		for(i=0;i<n;i++){
			if(i>=j){
				if(i==j){
					order = matrix[i][j];
					for(m=0;m<n;m++){
						matrix[i][m] = matrix[i][m] / order;
					}
         		 dizi[i] = dizi[i] / order;
				}
   			    else{
         			 carpim = matrix[i][j];
         			 for(a=0;a<n;a++){
					 	 matrix[i][a] = matrix[i][a] - (carpim * matrix[j][a]);
				  		}
          			dizi[i] = dizi[i] - (carpim * dizi[j]);

       			 }
				
			}
		}
	}




  	for(i=n-1;i>=0;i--){
    	for(j=n-1;j>=0;j--){
      		if(j>=i){
        		if(i==j){
          			result[i] = dizi[i];
        		}
        		else{
          			dizi[i] = dizi[i] - (matrix[i][j]*result[j]);
        		}
  			}
    	}
  	}
  	printf("Sonuclar: \n");
  	for(i=0;i<n;i++){
    	printf("X%d =  %f ",i+1, result[i]);
  	}
	
	
	
}

void matrixInverse(){
	float newMatrix[MAX][MAX],matrix[MAX][MAX],order,carpim;
	int i,j,k=0,n,m,d,a,o,g,p;

  	printf("Matrix boyut degerini giriniz: \n");
	scanf("%d", &n);
	
	for(i=0; i<n; i++){
		for(j=0;j<n;j++){
			printf("%d, %d : \n", i, j);
				scanf("%f", &matrix[i][j]);
			}
		}

  
	// elementer matrix
	for(i=0; i<n; i++){
		for(j=0; j<n; j++){
			if(i == j){
				newMatrix[i][j] = 1;
			}
			else{
				newMatrix[i][j] = 0;
			}
		}
	}
	
	for(j=0;j<n;j++){
		for(i=0;i<n;i++){
			if(i>=j){
				if(i==j){
					order = matrix[i][j];
					for(m=0;m<n;m++){
						matrix[i][m] = matrix[i][m] / order;
						newMatrix[i][m] = newMatrix[i][m] / order;
					}
				}
        		else{
          			carpim = matrix[i][j];
          			for(a=0;a<n;a++){
					    matrix[i][a] = matrix[i][a] - (carpim * matrix[j][a]);
					    newMatrix[i][a] = newMatrix[i][a] - (carpim * newMatrix[j][a]);
				  	}
          		}				
			}  
		}
	}

  	

  	for(j=n-1;j>=0;j--){
    	for(i=n-2;i>=0;i--){
      		if(i<=j){
          		if(i != j){
            		carpim = matrix[i][j];
        			for(m=0;m<n;m++){
          				matrix[i][m] = matrix[i][m] - (carpim * matrix[j][m]);
          				newMatrix[i][m] = newMatrix[i][m] - (carpim * newMatrix[j][m]);
            		}
        		}
      		}
    	}
  	}

	
	printf("Matrisin Tersi: \n ");
		for(i=0;i<n;i++){
			for(j=0;j<n;j++){
				printf("%f  ", newMatrix[i][j]);
			}
			printf("\n");
		}
}


float bisectionMethod(float a, float b, float durmadeger,float dizi[], int derece){
	float negatif, pozitif, sayi, fsayi;
	if ((Function(dizi, derece, a)) * (Function(dizi, derece, b)) < 0){
		if(Function(dizi, derece, a) < 0){
			negatif = a;
			pozitif = b;
		}
		else{
			negatif = b;
			pozitif = a;
		}
	
		do{
			sayi = (negatif + pozitif) / 2;
			fsayi = (Function(dizi, derece, sayi));
			if(fsayi < 0){
				negatif = sayi;
			}
			else{
				pozitif = sayi;
			}
		
		}while(durmadeger < fabs(fsayi));
		
	}
	else{
		printf("Bu aralikta kok yok \n");
	}
	return sayi;
}



float regulaFalsi(float a, float b, float durmadeger,float dizi[], int derece){
	float fa, fb, sayi, fsayi;
	if ((Function(dizi, derece, a)) * (Function(dizi, derece, b)) < 0){
		do{
			fa = (Function(dizi, derece, a));
			fb = (Function(dizi, derece, b));
			
			sayi = (b*fa-a*fb)/(fa-fb);
			fsayi = (Function(dizi, derece, sayi));
		
		
			if(fa * fsayi > 0){
				a = sayi;
			}
			else{
				b = sayi;
			}
		
	
		}while (durmadeger < fabs(fsayi));
	}	
	else{
		printf("Bu aralikta kok yok \n");
	}
		
	
	
	return sayi;
}



float newtonRaphson(float a, float b, float durmadeger,float dizi[], int derece, float baslangic){
	float kontrol, sayi, x0,x1;
	x0 = baslangic;
	do{
		x1 = x0 - (Function(dizi, derece, x0) / FunctionDerivative(dizi, derece, x0));
		kontrol = x1 - x0;
		x0 = x1;
	}while(durmadeger < fabs(kontrol));
	return x1;
}

void GaussSeidel(int denk_say, float hata){
	int i,j,n=denk_say,k,y,flag = 0, counter = 1, p;
	float matris[MAX][MAX], basla[MAX],temp;
	
	for(i=0;i<denk_say;i++){
		for(j=0;j<=denk_say;j++){
			if(j==denk_say){
				printf("%d. denklemin cevabini giriniz: \n", i+1 ,j+1);
				scanf("%f", &matris[i][j]);
			}
			else{
				printf("%d. denklemin %d. katsayisini giriniz:  \n", i+1 ,j+1);
				scanf("%f", &matris[i][j]);
			}
		}
	}
	
	printf("Baslangic degerlerini giriniz: \n");
	for(i=0;i<n;i++){
		printf("%d. \n",i);
		scanf("%f", &basla[i]);
	}
	
	// burada kosegenleri en buyuk yaptim.
	for(i=0;i<n;i++){
		for(j=i+1;j<n;j++){
			if(abs(matris[i][i] < abs(matris[j][i]))){
				for(k=0;k<=n;k++){
					temp = matris[i][k];
					matris[i][k] = matris[j][k];
					matris[j][k] = temp;
				}
			}
		}
	}
	
	do{
		
		for(i=0;i<n;i++){
			
			y = basla[i];
			basla[i] = matris[i][n];
			for(j=0;j<n;j++){
					if(i != j){
					basla[i] = basla[i] - (matris[i][j] * basla[j]);
				}
			}
			basla[i] = basla[i] / matris[i][i];
			
			if(abs(basla[i] - y) <= hata){
				flag++;
			}
		}
		counter ++;
		printf("\n");
		
	}while(flag < n);
	
	printf("Cevaplar: \n ");
	for(i=0;i<n;i++){
		printf("x%d = %f\n", i,basla[i]);
	}
	
	
	
}

void Trapez(){
	int derece,i;
	float a,b,n,dizi[MAX],h,x,sum=0,b1;
	printf("Fonksiyonun derecesini giriniz: \n");
	scanf("%d", &derece);
	
	for(i=derece; i>=0; i--){
		printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
		scanf("%f", &dizi[i]);
	}
	
	printf("Alt siniri giriniz: \n");
	scanf("%f", &a);
	printf("Ust siniri giriniz: \n");
	scanf("%f", &b);
	printf("n'i giriniz: \n");
	scanf("%f", &n);
	
	h = (b-a) / n;
	b1 = a + h;
	
	while(b1 != b+h){
		sum = sum + ((b1-a) * ((Function(dizi,derece,a) + Function(dizi,derece,b1)) / 2));
		a = b1;
		b1 = b1 + h;
	}
	
	
	printf("Sonuc : %lf \n", sum);
		
}

void Simpsonsbiruc(){
	int derece,i;
	float a,b,n,dizi[MAX],h,x,sum=0,b1,x1;
	printf("Fonksiyonun derecesini giriniz: \n");
	scanf("%d", &derece);
	
	for(i=derece; i>=0; i--){
		printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
		scanf("%f", &dizi[i]);
	}
	
	printf("Alt siniri giriniz: \n");
	scanf("%f", &a);
	printf("Ust siniri giriniz: \n");
	scanf("%f", &b);
	printf("n'i giriniz (cift sayi olmali): \n");
	scanf("%f", &n);
	
	h = (b-a) / n;
	b1 = a + h;
	
	while(b1 != b+h){
		x1 =(a+b1)/2;
		sum = sum + ((b1-a) *(((Function(dizi,derece,a)) + (4 * (Function(dizi,derece,x1)))+ (Function(dizi,derece,b1)) )/ 6 ));
		a = b1;
		b1 = b1 + h;
	}
	
	
	printf("Sonuc : %lf \n", sum);
}




void GregoryNewton(float x){
	
	int n,m,i,j;
  	float carpim = 1, sum,h;


  	printf("Kac adet veri oldugunu giriniz: \n");
  	scanf("%d", &n);
  	m=n-1;


  	float matris[n][n+2];

  	for(i=0;i<n;i++){
  		printf("x%d: \n",i);
  		scanf("%f", &matris[i][0]);
    	printf("y%d: \n", i);
    	scanf("%f", &matris[i][1]);
  	}

  	for(j=2;j<n+1;j++){
    	for(i=0;i<m;i++){
      	matris[i][j] = matris[i+1][j-1] - matris[i][j-1];
    	}
    	m--;
  	}
  	
  	
  	h= matris[1][0] - matris[0][0];

  	
  	sum = matris[0][1];
  	carpim = ((x-(matris[0][0]))/h) * matris[0][2];
  	sum = sum + carpim;
  	carpim = 1;
  	
  	m=1;

  	for(i=2;i<n;i++){
    	carpim = matris[0][i+1];
    	for(j=m;j>=0;j--){
      	carpim = carpim * (x-(matris[j][0]));
    	}
    	carpim = carpim / factorial(i);
    	carpim = carpim / pow(h,i);
    	sum = sum + carpim;
    	m++;
    	carpim = 1;
  	}
  	printf("%f",sum);
}






int main(int argc, char *argv[]) {
	int derece, i,j, karar,n,denk_say,yontem,islem;
	float dizi[MAX], result, a, b,durmadeger, nokta, h, baslangic,matrix[MAX][MAX],hata,x;
	
	
	printf("Yapmak Istediginiz Islemi Seciniz: \n");
	printf("1.Bisection Yontemi \n2.Regula-Falsi Yontemi\n3.Newton-Rapshon Yontemi \n4.NxN'lik Bir Matrisin Tersi\n5.Gauss Eleminasyon\n6.Gauss Seidal Yontemleri\n7.Sayisal Turev\n8.Simpson Yontemi\n9.Trapez Yontemi\n10.Degisken Donusumsuz Gregory Newton Enterpolasyonu\n ");
  	scanf("%d",&islem);
  	switch (islem){
  		case 1:
  			//bisection
  			printf("Denklemin derecesini giriniz: \n");
			scanf("%d", &derece);
	
	
			for(i=derece; i>=0; i--){
				printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
				scanf("%f", &dizi[i]);
			}
			int sayi = 1;

			printf("Aralik icin alt siniri giriniz: \n");
			scanf("%f", &a);
			printf("Aralik icin ust siniri giriniz: \n");
			scanf("%f", &b);
			printf("Durma degerini giriniz: \n");
			scanf("%f", &durmadeger);
			result = bisectionMethod(a, b, durmadeger, dizi, derece);
			printf("%f\n", result);
			system("pause");
			break;
			
		case 2:
			//regula falsi
			printf("Denklemin derecesini giriniz: \n");
			scanf("%d", &derece);
	
	
			for(i=derece; i>=0; i--){
				printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
				scanf("%f", &dizi[i]);
			}

			printf("Aralik icin alt siniri giriniz: \n");
			scanf("%f", &a);
			printf("Aralik icin ust siniri giriniz: \n");
			scanf("%f", &b);
			printf("Durma degerini giriniz: \n");
			scanf("%f", &durmadeger);
			result = regulaFalsi(a, b, durmadeger, dizi, derece);
			printf("%f\n", result);
			system("pause");
			break;
			
		case 3:
			//newton raphson
			printf("Denklemin derecesini giriniz: \n");
			scanf("%d", &derece);
	
	
			for(i=derece; i>=0; i--){
				printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
				scanf("%f", &dizi[i]);
			}

			printf("Aralik icin alt siniri giriniz: \n");
			scanf("%f", &a);
			printf("Aralik icin ust siniri giriniz: \n");
			scanf("%f", &b);
			printf("Durma degerini giriniz: \n");
			scanf("%f", &durmadeger);
			printf("Baslangic degerini giriniz: \n");
			scanf("%f", &baslangic);
			printf("%f", newtonRaphson(a,b,durmadeger,dizi,derece,baslangic));
			system("pause");
			
			break;
		
		case 4:
			//matrisin tersini alma
			matrixInverse();
			system("pause");
			break;
			
		case 5:
			//Gauss Eliminasyon
			GaussElimination();
			system("pause");
			break;
		
		case 6:
			
			printf("Denklem sayisini giriniz: \n");
  			scanf("%d", &denk_say);
  			printf("Durulmasi gereken hatayi giriniz: \n");
  			scanf("%f", &hata);
  	
  			GaussSeidel(denk_say,hata);
  			system("pause");
  			break;
  			
  		case 7:
  			//turev
  			printf("Denklemin derecesini giriniz: \n");
			scanf("%d", &derece);
			//polinom alýyorum 
	
			for(i=derece; i>=0; i--){
				printf("%d. derecedeki terimin katsayisini giriniz: \n", i);
				scanf("%f", &dizi[i]);
			}

  			printf("geri icin 1'i, ileri icin 2'yi, merkezi turev icin 3'u tuslayiniz: \n");
			scanf("%d", &karar);
			printf("noktayi giriniz:\n");
			scanf("%f", &nokta);
			printf("h'i giriniz: \n");
			scanf("%f", &h);
			printf("%f", derivate(dizi,derece,nokta,h,karar));
			printf("\n");
			system("pause");
			break;
			
		case 8:
			//simpson
  		
			Simpsonsbiruc();
			printf("\n");
			system("pause");
			break;
			
		case 9:
			Trapez();
			printf("\n");
			system("pause");
			break;
			
		case 10:
			printf("Gregory Newton Enterpolasyonu icin deger: \n");
			scanf("%f", &x);
			GregoryNewton(x);
			printf("\n");
			system("pause");
			break;
			
		default:
			printf("Gecerli bir yontem giriniz! \n");
			break; 
  			
	  }
	  
	  
  	

	return 0;
}


