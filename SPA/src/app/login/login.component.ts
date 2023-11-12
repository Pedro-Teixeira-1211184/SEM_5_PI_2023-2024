import {Component, inject, OnInit} from '@angular/core';
import {AuthService} from "../services/auth.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  title = 'LOGIN';

  auth: AuthService = inject(AuthService);
  loginForm!: FormGroup;

  ngOnInit(): void {
    this.loginForm = new FormGroup({
      email: new FormControl('', [Validators.required]),
      password: new FormControl('', [Validators.required])
    });
  }

  get email() {
    return this.loginForm.get('email');
  }

  get password() {
    return this.loginForm.get('password');
  }

  constructor(authService: AuthService) {
  }

  public async submit() {
    try {
      if (this.loginForm.invalid) {
        alert('Please fill all the fields');
        return;
      }
      await this.auth.login(this.email?.value, this.password?.value);
    } catch (e) {
      console.log(e);
    }
  }
}
