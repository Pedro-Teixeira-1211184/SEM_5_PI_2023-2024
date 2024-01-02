import {Injectable} from '@angular/core';
import Constants from "../../utils/Constants";
import {IUserDTO} from "../dto/IUserDTO";

@Injectable({
  providedIn: 'root'
})
export class AuthService {

  //user logged in
  public userEmail: string | null = null;

  constructor() {
  }

  public async login(email: string, password: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_AUTH_LOGIN_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          email: email,
          password: password
        })
      });

      if (response.status === 200) {
        const data = await response.json();
        //redirect to home page of a certain user role
        localStorage.setItem('token', data.token);
        localStorage.setItem('role', data.userDTO.role);
        localStorage.setItem('email', data.userDTO.email);
        localStorage.setItem('firstName', data.userDTO.firstName);
        localStorage.setItem('lastName', data.userDTO.lastName);
        localStorage.setItem('nif', data.userDTO.nif);
        window.location.href = '/home';
      } else {
        alert('Invalid credentials');
        window.location.href = '/';
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async logout(): Promise<void> {
    const response = await fetch(Constants.API_AUTH_LOGOUT_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    if (response.status === 200) {
      this.clearLocalStorage();
      window.location.href = '/login';
    } else {
      alert('Logout failed');
    }
  }

  public async isAuthenticated(): Promise<boolean> {
    return localStorage.getItem('token') !== null;
  }

  public async isAuthenticatedUser(): Promise<IUserDTO> {
    return {
      firstName: localStorage.getItem('firstName') as string,
      lastName: localStorage.getItem('lastName') as string,
      nif: '',
      email: localStorage.getItem('email') as string,
      role: localStorage.getItem('role') as string,
      password: ''
    };
  }

  public async authenticatedUserRole(): Promise<string> {
    return localStorage.getItem('role') as string;
  }

  public async deleteUser(email: string): Promise<void> {
    const response = await fetch(Constants.API_AUTH_DELETE_ACCOUNT_URL + email, {
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    if (response.status === 200) {
      this.clearLocalStorage();
      window.location.href = '/login';
    } else {
      alert('Delete failed');
    }
  }

  public async signUp(firstName: string, lastName: string, nif: string, email: string, password: string, role: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_AUTH_SIGNUP_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          firstName: firstName,
          lastName: lastName,
          nif: nif,
          email: email,
          password: password,
          role: role
        })
      });

      if (response.status === 403) {
        alert('Sign up failed');
      } else {
        alert('Sign up successful');
        await fetch(Constants.API_REQUEST_DELETE_URL + email, {
          method: 'DELETE',
          headers: {
            'Content-Type': 'application/json'
          }
        });
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async deleteRequest(email: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_REQUEST_DELETE_URL + email, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      if (response.status === 200) {
        alert('Delete successful')
      } else {
        alert('Delete failed');
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async signupRequest(firstName: string, lastName: string, nif: string, email: string, password: string): Promise<void> {
    try {
      console.log(firstName, lastName, email, password);
      const response = await fetch(Constants.API_AUTH_SIGNUP_REQUEST_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          firstName: firstName,
          lastName: lastName,
          nif: nif,
          email: email,
          password: password,
        })
      });

      const json = await response.json();

      if (response.status === 403) {
        alert(json);
      } else {
        window.location.href = '/login';
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async download(email: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_AUTH_GET_USER_BY_EMAIL + email, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      if (response.status === 200) {
        const data = await response.json();
        //desencriptar password
        data.password = atob(data.password);
        const content = 'Email: ' + data.email + '\n' + 'Name: ' + data.firstName + ' ' + data.lastName + '\n' + 'Role: ' + data.role;
        const blob = new Blob([content], {type: 'text/plain'});

        const a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = 'profile.txt'; // Nome do arquivo a ser baixado

        document.body.appendChild(a);
        a.click();

        document.body.removeChild(a);
      } else {
        alert('Download failed');
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async update(email: string, firstName: string, lastName: string, nif: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_UPDATE_USER_URL + localStorage.getItem('email'), {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          firstName: firstName,
          lastName: lastName,
          email: email,
          nif: nif
        })
      });

      if (response.status === 200) {
        alert('Update successful');
        localStorage.setItem('firstName', firstName);
        localStorage.setItem('lastName', lastName);
        localStorage.setItem('nif', nif);
        localStorage.setItem('email', email);
      } else {
        alert('Update failed');
      }
    } catch (e) {
      console.log(e);
    }
  }

  clearLocalStorage(): void {
    localStorage.removeItem('token')
    localStorage.removeItem('role')
    localStorage.removeItem('email')
    localStorage.removeItem('firstName')
    localStorage.removeItem('lastName')
    localStorage.removeItem('nif')
  }

}
