#include <functional>
#include <iostream>
#include <cmath>

float funcion(float x){
	// Modifique la expresión a evaluar
	return std::exp(-x) - x;
}

float algoritmoBiseccion(float, float, std::function<float(float)> );

int main(){
	float x;
	x = algoritmoBiseccion(-2, 1, &funcion);	
	std::cout << "La raiz es: " << x << std::endl;
	return 0;
}


float algoritmoBiseccion(float a, float b, std::function<float(float)> expr){
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
		r = (a + b)/2;

		if(expr(r) == 0) {
			return r;
		}else{ 
			if (expr(a)*expr(r) < 0) {
				r_1 = b;
				b = r;	
			}else {
				r_1 = a;
				a = r;
			}
		}
		error = std::abs((expr(r_1) - expr(r))/expr(r));

		std::cout << a << "\t" << b << "\t" << r << "\t\t" << error << "\n";
	}while(error > 0.05);

	return r;
}
