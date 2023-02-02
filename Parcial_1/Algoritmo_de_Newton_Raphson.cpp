#include <iostream>
#include <functional>
#include <cmath>
#include <ctime>
#include <cstdlib>

float funcion(float x){
	// Modifique la expresion a evaluar
	return pow(x, 2) - 4;
}

float derivada(float x){
	// Modifique la expresion a evaluar
	// Nota: regrese la expresion de la derivada escrita
	// en funcion
	return 2*x;
}

// tolerancia es un valor 0 <= x < 1
// funcion es una funcion de una variable, se pasa la direccion de memoria de la funcion (&funcion)
// derivada es la derivada de la funcion pasada, se pasa la direccion de memoria de la funcion (&derivada)
// NOTA: Si la funcion tiene mas de una raiz el algoritmo puede dar una u otra raiz dependeindo
// 		 de la ejecucion del programa
float newtonRaphson(float tolerancia, std::function<float(float)> funcion, std::function<float(float)> derivada);



int main(){
	float x;
	
	std::cout << "La raiz es: " << newtonRaphson(0.05, &funcion, &derivada) << std::endl;

	return 0;
}




float newtonRaphson(float tolerancia, std::function<float(float)> funcion, std::function<float(float)> derivada){
	srand(time(nullptr));
	float x_i;
	float dx = 1;    // Se inicializa en el error maximo
	
	// Se inicializa x_i en un valor entero aleatorio entero [-9, 9]
	(rand()%2 == 0) ? (x_i = rand()%10) : (x_i = -1*rand()%10);

	while(std::abs(dx) > tolerancia){
		dx = funcion(x_i) / derivada(x_i);
		x_i = x_i - dx;
	}

	return x_i;
}
