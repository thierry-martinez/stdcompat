pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                sh 'autoreconf && ./configure && make'
            }
        }
        stage('Test') {
            steps {
                sh 'make tests'
            }
        }
        stage('Deploy') {
            steps {
                sh 'make dist'
                archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
            }
        }
    }
}
