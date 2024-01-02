import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {AuthService} from "../../../services/auth.service";

@Component({
  selector: 'app-update',
  templateUrl: './update.component.html',
  styleUrls: ['./update.component.css']
})
export class UpdateComponent implements OnInit {

  service: AuthService = inject(AuthService)

  form!: FormGroup;

  constructor() {
  }

  ngOnInit(): void {
    this.form = new FormGroup({
      firstName: new FormControl(localStorage.getItem('firstName'), Validators.required),
      lastName: new FormControl(localStorage.getItem('lastName'), Validators.required),
      nif: new FormControl(localStorage.getItem('nif'), Validators.required),
      email: new FormControl(localStorage.getItem('email'), Validators.required),
      role: new FormControl(localStorage.getItem('role'), Validators.required)
    });
  }

  get firstName() {
    return this.form.get('firstName');
  }

  get lastName() {
    return this.form.get('lastName');
  }

  get nif() {
    return this.form.get('nif');
  }

  get email() {
    return this.form.get('email');
  }

  get role() {
    return this.form.get('role');
  }

  onSubmit() {
    this.service.update(this.email?.value, this.firstName?.value, this.lastName?.value, this.nif?.value);
  }

  protected readonly localStorage = localStorage;
}
