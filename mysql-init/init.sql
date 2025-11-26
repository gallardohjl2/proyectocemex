ALTER USER 'root'@'%' IDENTIFIED WITH mysql_native_password BY 'admin1234';
FLUSH PRIVILEGES;


int[] numeros = {2, 5, 8, 10, 13, 18, 23, 30, 35, 40};

// Mostrar la lista ordenada String lista = "Lista ordenada:\n";
for (int num : numeros) {
lista += num + " ";
}
JOptionPane.showMessageDialog(null, lista);

// Pedir al usuario el número a buscar int valorBuscado = Integer.parseInt(
JOptionPane.showInputDialog("Ingresa el número que deseas buscar:")
);

int inicio = 0;
int fin = numeros.length - 1;
boolean encontrado = false;

while (inicio <= fin) {
int medio = (inicio + fin) / 2; // posición central 
if (numeros[medio] == valorBuscado) {
JOptionPane.showMessageDialog(null,
"El número " + valorBuscado +
" se encuentra en la posición " + medio);
encontrado = true;
break;
}
else if (valorBuscado < numeros[medio]) {
fin = medio - 1; // mitad izquierda }
else {
inicio = medio + 1; // mitad derecha }
}

if (!encontrado) {
JOptionPane.showMessageDialog(null,
" El número " + valorBuscado +
" no se encuentra en la lista.");
}
}
}
