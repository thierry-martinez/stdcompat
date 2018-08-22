pipeline {
    agent {
        dockerfile {
            label 'slave'
        }
    }

    stages {
        stage('Build') {
            steps {
                sh 'eval `opam config env` && autoreconf && mkdir build && cd build && ../configure && make'
            }
        }
        stage('Test') {
            steps {
                script {
                    def branches = [:]
                    for (i in ['3.07', '3.08.4', '3.09.3']) {
                        def switch_name = i
                        branches[switch_name] = {
                            sh "opam switch $switch_name && eval `opam config env` && mkdir build/$switch_name && cd build/$switch_name && ../../configure && make && make tests"
                        }
                    }
                    parallel branches
                }
            }
        }
        stage('Deploy') {
            steps {
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
