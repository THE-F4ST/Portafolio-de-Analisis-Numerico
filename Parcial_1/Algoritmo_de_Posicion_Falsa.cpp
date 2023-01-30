#include <functional>
#include <iostream>
#include <iomanip>
#include <cmath>

float funcion(float x){
	// Modifique la expresión a evaluar
	return std::exp(-x) - x;
}

float algoritmoPosicionFalsa(float, float, float, std::function<float(float)> );



int main(){
	float x;
	x = algoritmoPosicionFalsa(-2, 1, 0.05, &funcion);	
	std::cout << "La raiz es: " << std::setprecision(6) << x << std::endl;
	return 0;
}




float algoritmoPosicionFalsa(float a, float b, float tolerancia, std::function<float(float)> expr){
	float r, r_1;
	float error = 1;
	
	// Validación de intervalo
	if(expr(a)*expr(b) > 0) {
		std::cout << "ERROR: Intervalo incorrecto\n";
		return INFINITY;
	}

	// En caso de que a y b sean raices únicamente regresa a
	if(expr(a)*expr(b) == 0) {
		std::cout << "Alerta: " << a << "o " << b << "son raices\n";
		if(expr(a) == 0){
			return a;
		}else{
			return b;
		}
	}

	// Inicia algoritmo de biseccion
	std::cout << "a\t\tb\t\tr\t\t\terror\n";
	std::cout << a << "\t\t" << b << "\t\t" << r << "\t\t" << error << "\n";
	do{
		// Punto medio entre a y b
		r = a + (b - a)/2;

	if(expr(r) == 0) {
			return r;
		}else{ 
			if (expr(a)*expr(r) < 0) {
				r_1 = b - (expr(b)*(a-b))/(expr(a)-expr(b));
				b = r;	
			}else {
				r_1 = b - (expr(b)*(a-b))/(expr(a)-expr(b));
				a = r;
			}
		}
		error = std::abs((expr(r_1) - expr(r))/expr(r));

		std::cout << a << "\t" << b << "\t" << r << "\t\t" << error << "\n";
	}while(error > tolerancia);

	return r;
}
