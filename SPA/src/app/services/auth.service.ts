import {Injectable} from '@angular/core';
import Constants from "../../utils/Constants";

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
        this.userEmail = data.email;
        //redirect to home page of a certain user role
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
      window.location.href = '/login';
    } else {
      alert('Logout failed');
    }
  }

  public async isAuthenticated(): Promise<boolean> {
    const response = await fetch(Constants.API_AUTH_AUTHENTICATED_URL, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    return response.status === 200;
  }

  public async authenticatedUserRole(): Promise<string> {
    const response = await fetch(Constants.API_AUTH_AUTHENTICATED_URL, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    if (response.status === 200) {
      const data = await response.json();
      return data.role;
    } else {
      return '';
    }
  }

  public async signUp(firstName: string, lastName: string, email: string, password: string, role: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_AUTH_SIGNUP_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          firstName: firstName,
          lastName: lastName,
          email: email,
          password: password,
          role: role
        })
      });

      if (response.status === 403) {
        alert('Sign up failed');
      } else {
        alert('Sign up successful');
        window.location.href = '/home';
      }
    } catch (e) {
      console.log(e);
    }
  }

}
