function login() {
    const email = document.getElementById("email").value;
    const password = document.getElementById("password").value;

    // Substitua a URL abaixo pela URL do seu backend
    const backendURL = "http://localhost:5050/api/auth/signin";

    fetch(backendURL, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify({ email: email, password: password }),
    })
        .then(response => {
            if (response.ok) {
                return response.json();
            } else {
                throw new Error('Credenciais inválidas');
            }
        })
        .then(data => {
            // Armazena as informações do usuário e o token na localStorage
            localStorage.setItem('email', data.email);
            localStorage.setItem('token', data.token);

            // Redireciona para a página "View.html"
            window.location.href = 'View.html';
        })
        .catch(error => {
            alert("Erro no login: " + error.message);
        });
}