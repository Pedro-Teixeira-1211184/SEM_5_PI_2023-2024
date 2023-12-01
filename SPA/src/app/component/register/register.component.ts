import {Component, inject} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {AuthService} from "../../services/auth.service";
import Constants from "../../../utils/Constants";

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.css']
})
export class RegisterComponent {
  ngOnInit(): void {
    this.form = new FormGroup({
      firstName: new FormControl('', Validators.required),
      lastName: new FormControl('', Validators.required),
      email: new FormControl('', [Validators.required, Validators.email]),
      password: new FormControl('', Validators.required)
    });
  }

  service: AuthService = inject(AuthService);

  form!: FormGroup;
  conditions = true;

  constructor() {
  }

  get firstName() {
    return this.form.get('firstName');
  }

  get lastName() {
    return this.form.get('lastName');
  }

  get email() {
    return this.form.get('email');
  }

  get password() {
    return this.form.get('password');
  }

  public async submit() {
    // just if first name and last name are not empty just as email and password
    if (!this.form.invalid) {
      //TODO: dont create a user but instead create a request to create a user
      await this.service.signUp(this.firstName?.value, this.lastName?.value, this.email?.value, this.password?.value, Constants.USER_ROLE);
    }
  }

  accept() {
    this.conditions = false;
  }

  decline() {
    window.location.href = "/home";
  }
}
